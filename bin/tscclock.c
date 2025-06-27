/* Copyright (c) 2022 Meng Rao <raomeng1@gmail.com>
 * Copyright (c) 2015 The mtime programmers
 * Copyright (c) 2024 Romain Calascibetta <romain.calascibetta@gmail.com>
 */

#include <stdatomic.h>
#include <stdint.h>

struct base {
  uint64_t tsc; // base
  uint64_t ns; // base
  double ns_per_tsc;
};

_Alignas(128) struct base _base = {0};
atomic_int_least32_t _seq = 0;
char _padding[128 - ((sizeof(struct base) + sizeof(_seq)) % 128)];

static void tsc_save(uint64_t base_tsc, uint64_t trust_ns, uint64_t base_ns_err,
                     double new_ns_per_sec) {
  uint32_t seq0 = atomic_load_explicit(&_seq, memory_order_relaxed);
  atomic_store_explicit(&_seq, seq0 + 1, memory_order_release);
  atomic_signal_fence(memory_order_acq_rel);
  _base.tsc = base_tsc;
  _base.ns = trust_ns + base_ns_err;
  _base.ns_per_tsc = new_ns_per_sec;
  atomic_signal_fence(memory_order_acq_rel);
  atomic_store_explicit(&_seq, seq0 + 2, memory_order_release);
}

static uint64_t tsc2ns(uint64_t tsc) {
  uint32_t seq0, seq1;
  uint64_t ns;

  do {
    seq0 = atomic_load_explicit(&_seq, memory_order_acquire);
    atomic_signal_fence(memory_order_acq_rel);
    ns = _base.ns + (uint64_t)((tsc - _base.tsc) * _base.ns_per_tsc);
    atomic_signal_fence(memory_order_acq_rel);
    seq1 = atomic_load_explicit(&_seq, memory_order_acquire);
  } while (seq0 != seq1 || seq0 & 1);

  return ns;
}

static uint64_t rdtsc() {
#if defined(__i386__)
  uint64_t ret;
  __asm__ __volatile__("rdtsc" : "=A"(ret));
  return ret;
#elif defined(__x86_64__) || defined(__amd64__)
  uint64_t hi, lo;
  __asm__ __volatile__("rdtsc" : "=a"(lo), "=d"(hi));
  return (lo | (hi << 32));
#elif defined(__powerpc__) || defined(__ppc__)
#if defined(__powerpc64__) || defined(__ppc64__)
  uint64_t tb;
  __asm__ __volatile__("mfspr %0, 268" : "=r"(tb));
  return tb;
#else
  uint32_t tbl, tu0, tbu1;
  __asm__ __volatile__("mftbu %0\n"
                       "mftbl %1\n"
                       "mftbu %2"
                       : "=r"(rbu0), "=r"(tbl), "=r"(tbu1));
  tbl &= (uint32_t)(tbu0 == tbu1);
  return (((uint64_t)tbu1 << 32) | ((uin64_t)tbl));
#endif
#elif defined(__ia64__)
  uint64_t itc;
  __asm__("mov %0 = ar.itc" : "=r"(itc));
  return itc;
#elif defined(__aarch64__)
  uint64_t virtual_timer_value;
  __asm__ __volatile__("mrs %0, cntvct_el0" : "=r"(virtual_timer_value));
  return virtual_timer_value;
#else
#error "A cycle timer is not available for your OS and CPU"
#endif
}

#if defined(__ocaml_solo5__)
#define UTIME_SOLO5
#include <solo5.h>
#elif defined(__APPLE__) && defined(__MACH__)
#define UTIME_DARWIN
#include <mach/mach_time.h>
static mach_timebase_info_data_t scale = {0};
#elif (defined(__unix__) || defined(__unix))
#define UTIME_POSIX
#include <unistd.h>
#if defined(_POSIX_MONOTONIC_CLOCK)
#include <time.h>
#else
#error "A monotonic clock is not available for your POSIX OS"
#endif
#elif defined(_WIN32)
#define UTIME_WINDOWS
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
static double performance_frequency;
#else
#error "A monotonic clock is not available for your OS"
#endif

#define UTIME_DARWIN_MACH_TIMEBASE_INFO_ERR (-1)
#define UTIME_DARWIN_MACH_DENOM_IS_ZERO (-2)
#define UTIME_WINDOWS_QUERY_FREQUENCY_ERR (-3)
#define UTIME_POSIX_GETTIME_ERR (-4)
#define UTIME_WINDOWS_QUERY_COUNTER_ERR (-5)
#define UTIME_TOO_MANY_RETRIES (-6)

static int rdsysns_init(void) {
#if defined(UTIME_SOLO5)
  return 0;
#elif defined(UTIME_DARWIN)
  if (mach_timebase_info(&scale) != KERN_SUCCESS)
    return UTIME_DARWIN_MACH_TIMEBASE_INFO_ERR;
  if (scale.denom == 0)
    return UTIME_DARWIN_MACH_DENOM_IS_ZERO;
  start = mach_continuous_time();
  return 0;
#elif defined(UTIME_POSIX)
  return 0;
#elif UTIME_WINDOWS
  LARGE_INTEGER t_freq;
  if (!QueryPerformanceFrequency(&t_freq))
    return UTIME_WINDOWS_QUERY_FREQUENCY_ERR;
  performance_frequency = (1000000000.0 / t_freq.QuadPart);
  return 0;
#endif
}

static uint64_t rdsysns() {
#if defined(UTIME_SOLO5)
  return (solo5_clock_wall());
#elif defined(UTIME_DARWIN)
  uint64_t now = mach_continuous_time();
  return ((now * scale.numer) / scale.denom);
#elif defined(UTIME_POSIX)
  struct timespec now;
  if (clock_gettime(CLOCK_MONOTONIC, &now))
    return UTIME_POSIX_GETTIME_ERR;
  return ((uint64_t)(now.tv_sec) * (uint64_t)1000000000 +
          (uint64_t)(now.tv_nsec));
#elif defined(UTIME_WINDOWS)
  static LARGE_INTEGER now;
  if (!QueryPerformanceCounter(&now))
    return UTIME_WINDOWS_QUERY_COUNTER_ERR;
  return (now.QuadPart * performance_frequency);
#endif
}

static uint64_t rdns() { return tsc2ns(rdtsc()); }

static int tsc_sync_time(uint64_t *tsc_out, uint64_t *ns_out) {
#ifdef _MSC_VER
  const int N = 15;
#else
  const int N = 3;
#endif

  int retry = 0;
  uint64_t tsc[N + 1];
  uint64_t ns[N + 1];

retry:
  if (retry >= 10)
    return UTIME_TOO_MANY_RETRIES;

  tsc[0] = rdtsc();
  for (int i = 1; i <= N; i++) {
    ns[i] = rdsysns();
    if (ns[i] < 0) {
      retry++;
      goto retry;
    }
    tsc[i] = rdtsc();
  }

#ifdef _MSC_VER
  int j = 1;
  for (int i = 2; i <= N; i++) {
    if (ns[i] == ns[i - 1])
      continue;
    tsc[j - 1] = tsc[i - 1];
    ns[j++] = ns[i];
  }
  j--;
#else
  int j = N + 1;
#endif

  int best = 1;
  for (int i = 2; i < j; i++) {
    if (tsc[i] - tsc[i - 1] < tsc[best] - tsc[best - 1])
      best = i;
  }

  (*tsc_out) = (tsc[best] + tsc[best - 1]) >> 1;
  (*ns_out) = ns[best];

  return 0;
}

#include <math.h>
#include <stdio.h>

static int tsc_init(uint64_t init_calibrate_ns) {
  uint64_t errno = 0;
  if ((errno = rdsysns_init()) != 0)
    return errno;
  uint64_t base_tsc, base_ns;
  tsc_sync_time(&base_tsc, &base_ns);
  uint64_t expire_ns = base_ns + init_calibrate_ns;
  while ((errno = rdsysns()) < expire_ns && errno >= 0)
    __sync_synchronize();
  if (errno < 0)
    return errno;
  uint64_t delayed_tsc, delayed_ns;
  tsc_sync_time(&delayed_tsc, &delayed_ns);
  double init_ns_per_tsc =
      (double)(delayed_ns - base_ns) / (delayed_tsc - base_tsc);
  tsc_save(base_tsc, base_ns, 0, init_ns_per_tsc);
  return 0;
}

#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#define __unit value unit __attribute__((unused))

uint64_t caml_utime_rdns(__unit) { return (rdns()); }

double caml_utime_set_freq(double new_ns_per_tsc) {
  uint32_t seq0 = atomic_load_explicit(&_seq, memory_order_relaxed);
  double old_ns_per_sec;
  atomic_store_explicit(&_seq, seq0 + 1, memory_order_release);
  atomic_signal_fence(memory_order_acq_rel);
  old_ns_per_sec = _base.ns_per_tsc;
  _base.ns_per_tsc = new_ns_per_tsc;
  atomic_signal_fence(memory_order_acq_rel);
  atomic_store_explicit(&_seq, seq0 + 2, memory_order_release);
  return old_ns_per_sec;
}

CAMLprim value caml_utime_init(uint64_t init_calibrate_ns) {
  switch (tsc_init(init_calibrate_ns)) {
  case UTIME_DARWIN_MACH_TIMEBASE_INFO_ERR:
    caml_failwith("mach_timebase_info() failed");
    break;
  case UTIME_DARWIN_MACH_DENOM_IS_ZERO:
    caml_failwith("mach_timebase_info().denom is 0");
    break;
  case UTIME_POSIX_GETTIME_ERR:
    caml_failwith("clock_gettime() failed");
    break;
  case UTIME_WINDOWS_QUERY_FREQUENCY_ERR:
    caml_failwith("QueryPerformanceFrequency() failed");
    break;
  case UTIME_WINDOWS_QUERY_COUNTER_ERR:
    caml_failwith("QueryPerformanceCounter() failed");
    break;
  case UTIME_TOO_MANY_RETRIES:
    caml_failwith("too many retries");
    break;
  default:
    break;
  }

  return Val_unit;
}

double caml_utime_get_freq(__unit) {
  uint32_t seq0, seq1;
  double ns_per_tsc;

  do {
    seq0 = atomic_load_explicit(&_seq, memory_order_acquire);
    atomic_signal_fence(memory_order_acq_rel);
    ns_per_tsc = _base.ns_per_tsc;
    atomic_signal_fence(memory_order_acq_rel);
    seq1 = atomic_load_explicit(&_seq, memory_order_acquire);
  } while (seq0 != seq1 || seq0 & 1);

  return (1.0 / ns_per_tsc);
}
