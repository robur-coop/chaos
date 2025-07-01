/*
 * Copyright (C) Romain Calascibetta 2025
 * Copyright (C) Richard P. Curnow  1997-2003
 * Copyright (C) Miroslav Lichvar  2011, 2016-2017
 */

#include <assert.h>
#include <math.h>
#include <string.h>

#define MAX_POINTS 64
#define REGRESS_RUNS_RATIO 2
#define MIN_SAMPLES_FOR_REGRESS 3
#define EXCH(a, b)                                                             \
  temp = (a);                                                                  \
  (a) = (b);                                                                   \
  (b) = temp

/* ================================================== */
/* Critical value for number of runs of residuals with same sign.
   5% critical region for now. */

static char critical_runs[] = {
    0,  0,  0,  0,  0,  0,  0,  0,  2,  3,  3,  3,  4,  4,  5,  5,  5,  6,  6,
    7,  7,  7,  8,  8,  9,  9,  9,  10, 10, 11, 11, 11, 12, 12, 13, 13, 14, 14,
    14, 15, 15, 16, 16, 17, 17, 18, 18, 18, 19, 19, 20, 20, 21, 21, 21, 22, 22,
    23, 23, 24, 24, 25, 25, 26, 26, 26, 27, 27, 28, 28, 29, 29, 30, 30, 30, 31,
    31, 32, 32, 33, 33, 34, 34, 35, 35, 35, 36, 36, 37, 37, 38, 38, 39, 39, 40,
    40, 40, 41, 41, 42, 42, 43, 43, 44, 44, 45, 45, 46, 46, 46, 47, 47, 48, 48,
    49, 49, 50, 50, 51, 51, 52, 52, 52, 53, 53, 54, 54, 55, 55, 56};

/* ================================================== */

static int n_runs_from_residuals(double *resid, int n) {
  int nruns;
  int i;

  nruns = 1;
  for (i = 1; i < n; i++) {
    if (((resid[i - 1] < 0.0) && (resid[i] < 0.0)) ||
        ((resid[i - 1] > 0.0) && (resid[i] > 0.0))) {
      /* Nothing to do */
    } else {
      nruns++;
    }
  }

  return nruns;
}

/* ================================================== */
/* Return a boolean indicating whether we had enough points for
   regression */

int RGR_FindBestRegression(
    double *x, /* independent variable */
    double *y, /* measured data */
    double *w, /* weightings (large => data
                  less reliable) */

    int n,           /* number of data points */
    int m,           /* number of extra samples in x and y arrays
                        (negative index) which can be used to
                        extend runs test */
    int min_samples, /* minimum number of samples to be kept after
                        changing the starting index to pass the runs
                        test */

    /* And now the results */

    double *b0, /* estimated y axis intercept */
    double *b1, /* estimated slope */
    double *s2, /* estimated variance of data points */

    double *sb0, /* estimated standard deviation of
                    intercept */
    double *sb1, /* estimated standard deviation of
                    slope */

    int *new_start, /* the new starting index to make the
                       residuals pass the two tests */

    int *n_runs, /* number of runs amongst the residuals */

    int *dof /* degrees of freedom in statistics (needed
                to get confidence intervals later) */

) {
  double P, Q, U, V, W; /* total */
  double resid[MAX_POINTS * REGRESS_RUNS_RATIO];
  double ss;
  double a, b, u, ui, aa;

  int start, resid_start, nruns, npoints;
  int i;

  assert(n <= MAX_POINTS && m >= 0);
  assert(n * REGRESS_RUNS_RATIO <
         sizeof(critical_runs) / sizeof(critical_runs[0]));

  if (n < MIN_SAMPLES_FOR_REGRESS) {
    return 0;
  }

  start = 0;
  do {

    W = U = 0;
    for (i = start; i < n; i++) {
      U += x[i] / w[i];
      W += 1.0 / w[i];
    }

    u = U / W;

    P = Q = V = 0.0;
    for (i = start; i < n; i++) {
      ui = x[i] - u;
      P += y[i] / w[i];
      Q += y[i] * ui / w[i];
      V += ui * ui / w[i];
    }

    b = Q / V;
    a = (P / W) - (b * u);

    /* Get residuals also for the extra samples before start */
    resid_start = n - (n - start) * REGRESS_RUNS_RATIO;
    if (resid_start < -m)
      resid_start = -m;

    for (i = resid_start; i < n; i++) {
      resid[i - resid_start] = y[i] - a - b * x[i];
    }

    /* Count number of runs */
    nruns = n_runs_from_residuals(resid, n - resid_start);

    if (nruns > critical_runs[n - resid_start] ||
        n - start <= MIN_SAMPLES_FOR_REGRESS || n - start <= min_samples) {
      if (start != resid_start) {
        /* Ignore extra samples in returned nruns */
        nruns = n_runs_from_residuals(resid + (start - resid_start), n - start);
      }
      break;
    } else {
      /* Try dropping one sample at a time until the runs test passes. */
      ++start;
    }

  } while (1);

  /* Work out statistics from full dataset */
  *b1 = b;
  *b0 = a;

  ss = 0.0;
  for (i = start; i < n; i++) {
    ss += resid[i - resid_start] * resid[i - resid_start] / w[i];
  }

  npoints = n - start;
  ss /= (double)(npoints - 2);
  *sb1 = sqrt(ss / V);
  aa = u * (*sb1);
  *sb0 = sqrt((ss / W) + (aa * aa));
  *s2 = ss * (double)npoints / W;

  *new_start = start;
  *dof = npoints - 2;
  *n_runs = nruns;

  return 1;
}

/* ================================================== */
/* Find the index'th biggest element in the array x of n elements.
   flags is an array where a 1 indicates that the corresponding entry
   in x is known to be sorted into its correct position and a 0
   indicates that the corresponding entry is not sorted.  However, if
   flags[m] = flags[n] = 1 with m<n, then x[m] must be <= x[n] and for
   all i with m<i<n, x[m] <= x[i] <= x[n].  In practice, this means
   flags[] has to be the result of a previous call to this routine
   with the same array x, and is used to remember which parts of the
   x[] array we have already sorted.

   The approach used is a cut-down quicksort, where we only bother to
   keep sorting the partition that contains the index we are after.
   The approach comes from Numerical Recipes in C (ISBN
   0-521-43108-5). */

static double find_ordered_entry_with_flags(double *x, int n, int index,
                                            char *flags) {
  int u, v, l, r;
  double temp;
  double piv;
  int pivind;

  assert(index >= 0);

  /* If this bit of the array is already sorted, simple! */
  if (flags[index]) {
    return x[index];
  }

  /* Find subrange to look at */
  u = v = index;
  while (u > 0 && !flags[u])
    u--;
  if (flags[u])
    u++;

  while (v < (n - 1) && !flags[v])
    v++;
  if (flags[v])
    v--;

  do {
    if (v - u < 2) {
      if (x[v] < x[u]) {
        EXCH(x[v], x[u]);
      }
      flags[v] = flags[u] = 1;
      return x[index];
    } else {
      pivind = (u + v) >> 1;
      EXCH(x[u], x[pivind]);
      piv = x[u]; /* New value */
      l = u + 1;
      r = v;
      do {
        while (l < v && x[l] < piv)
          l++;
        while (r > 0 && x[r] > piv)
          r--;
        if (r <= l)
          break;
        EXCH(x[l], x[r]);
        l++;
        r--;
      } while (1);
      EXCH(x[u], x[r]);
      flags[r] = 1; /* Pivot now in correct place */
      if (index == r) {
        return x[r];
      } else if (index < r) {
        v = r - 1;
      } else if (index > r) {
        u = l;
      }
    }
  } while (1);
}

/* ================================================== */

#if 0
/* Not used, but this is how it can be done */
static double
find_ordered_entry(double *x, int n, int index)
{
  char flags[MAX_POINTS];

  memset(flags, 0, n * sizeof (flags[0]));
  return find_ordered_entry_with_flags(x, n, index, flags);
}
#endif

/* ================================================== */
/* Find the median entry of an array x[] with n elements. */

static double find_median(double *x, int n) {
  int k;
  char flags[MAX_POINTS];

  memset(flags, 0, n * sizeof(flags[0]));
  k = n >> 1;
  if (n & 1) {
    return find_ordered_entry_with_flags(x, n, k, flags);
  } else {
    return 0.5 * (find_ordered_entry_with_flags(x, n, k, flags) +
                  find_ordered_entry_with_flags(x, n, k - 1, flags));
  }
}

/* ================================================== */
/* This routine performs linear regression with two independent variables.
   It returns non-zero status if there were enough data points and there
   was a solution. */

int multiple_regress(double *x1, /* first independent variable */
                     double *x2, /* second independent variable */
                     double *y,  /* measured data */

                     int n, /* number of data points */

                     /* The results */
                     double *b2 /* estimated second slope */
                                /* other values are not needed yet */
) {
  double Sx1, Sx2, Sx1x1, Sx1x2, Sx2x2, Sx1y, Sx2y, Sy;
  double U, V, V1, V2, V3;
  int i;

  if (n < 4)
    return 0;

  Sx1 = Sx2 = Sx1x1 = Sx1x2 = Sx2x2 = Sx1y = Sx2y = Sy = 0.0;

  for (i = 0; i < n; i++) {
    Sx1 += x1[i];
    Sx2 += x2[i];
    Sx1x1 += x1[i] * x1[i];
    Sx1x2 += x1[i] * x2[i];
    Sx2x2 += x2[i] * x2[i];
    Sx1y += x1[i] * y[i];
    Sx2y += x2[i] * y[i];
    Sy += y[i];
  }

  U = n * (Sx1x2 * Sx1y - Sx1x1 * Sx2y) + Sx1 * Sx1 * Sx2y - Sx1 * Sx2 * Sx1y +
      Sy * (Sx2 * Sx1x1 - Sx1 * Sx1x2);

  V1 = n * (Sx1x2 * Sx1x2 - Sx1x1 * Sx2x2);
  V2 = Sx1 * Sx1 * Sx2x2 + Sx2 * Sx2 * Sx1x1;
  V3 = -2.0 * Sx1 * Sx2 * Sx1x2;
  V = V1 + V2 + V3;

  /* Check if there is a (numerically stable) solution */
  if (fabs(V) * 1.0e10 <= -V1 + V2 + fabs(V3))
    return 0;

  *b2 = U / V;

  return 1;
}

/* ================================================== */

#include <caml/memory.h>
#include <caml/mlvalues.h>

float regress_find_median(value x, int n) {
  double tmp[MAX_POINTS];

  assert(n > 0 && n <= MAX_POINTS);
  memcpy(tmp, (const double *)x, n * sizeof(tmp[0]));

  return find_median(tmp, n);
}

#include <stdio.h>

int regress_find_best_regression(int runs_samples, int n_samples,
                                 int min_samples, value times_back,
                                 value offsets, value weights, value est,
                                 value vres) {
  double est_intercept = 0.0, est_slope = 0.0, est_var = 0.0,
         est_intercept_sd = 0.0, est_slope_sd = 0.0;
  int32_t best_start = 0, nruns = 0, degrees_of_freedom = 0;
  int regression_ok = 0;

  regression_ok = RGR_FindBestRegression(
      (double *)times_back + runs_samples, (double *)offsets + runs_samples,
      (double *)weights, n_samples, runs_samples, min_samples, &est_intercept,
      &est_slope, &est_var, &est_intercept_sd, &est_slope_sd, &best_start,
      &nruns, &degrees_of_freedom);
  Store_double_flat_field(est, 0, est_intercept);
  Store_double_flat_field(est, 1, est_slope);
  Store_double_flat_field(est, 2, est_var);
  Store_double_flat_field(est, 3, est_intercept_sd);
  Store_double_flat_field(est, 4, est_slope_sd);

  int32_t *res = (int32_t *)Bytes_val(vres);
  memcpy(&res[0], (int32_t *)&best_start, sizeof(int32_t));
  memcpy(&res[1], (int32_t *)&nruns, sizeof(int32_t));
  memcpy(&res[2], (int32_t *)&degrees_of_freedom, sizeof(int32_t));

  return regression_ok;
}

int regress_multiple_regress(value x1, value x2, value y, int n, value res) {
  double b2;
  int result;

  result = multiple_regress((double *)x1, (double *)x2, (double *)y, n, &b2);
  memcpy(Bytes_val(res), (double *)&b2, sizeof(double));

  return result;
}
