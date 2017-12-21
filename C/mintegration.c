#include "cubature.h"

double mintegration(
  char     version,
  int      f(unsigned, const double*, void*, unsigned, double*),
  unsigned dim,
  double*  xmin,
  double*  xmax,
  double   relError,
  double*  errorEstimate
)
{
  double value;
  switch(version){
	  case 'h':
      hcubature(
        1, f, NULL, dim, xmin, xmax, 0, 0,
        relError, ERROR_INDIVIDUAL, &value, errorEstimate
      );
      break;
    case 'p':
      pcubature(
        1, f, NULL, dim, xmin, xmax, 0, 0,
        relError, ERROR_INDIVIDUAL, &value, errorEstimate
      );
  }
  return value;
}
