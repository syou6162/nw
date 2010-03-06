#include <R.h> 
#include <Rinternals.h> 

double epanechnikov(double X1, double x1, double X2, double x2,
		    double h11, double h12, double h21, double h22)
{
  double det = h11 * h22 - h12 * h21;
  double u1 = 1 / det * (h22 * (x1 - X1) - h12 * (x2 - X2));
  double u2 = 1 / det * (-h21 * (x1 - X1) + h11 * (x2 - X2));
  double utu = sqrt(u1*u1 + u2*u2);
  if(-1.0 < utu && utu < 1.0){
    return utu;
  }else{
    return 0;
  }
}

double g_i(double *y,double *X1,double x1,double *X2,double x2,
	   int n,double h11,double h12,double h21,double h22,int i)
{
  double sum = 0;
  for(int j=0;j<n;j++){
    if(j!=i) sum += epanechnikov(X1[j],x1,X2[j],x2,h11,h12,h21,h22) * y[j];
  }
  return sum;
}

double f_i(double *X1,double x1,double *X2,double x2,
	   int n,double h11,double h12,double h21,double h22,int i)
{
  double sum = 0;
  for(int j=0;j<n;j++){
    if(j!=i) sum += epanechnikov(X1[j],x1,X2[j],x2,h11,h12,h21,h22);
  }
  return sum;
}

double m_i(double *y,double *X1,double x1,double *X2,double x2,
	   int n,double h11,double h12,double h21,double h22,int i)
{
  return g_i(y,X1,x1,X2,x2,n,h11,h12,h21,h22,i) / f_i(X1,x1,X2,x2,n,h11,h12,h21,h22,i);
}

void cv(double *y,double *X1,double *X2,int *n,
	double *h11,double *h12,double *h21,double *h22,double *result)
{
  int i,j;
  for(i =0;i<*n;i++){
    *result = *result + pow(y[i] - m_i(y,X1,X1[i],X2,X2[i],*n,*h11,*h12,*h21,*h22,i),2);
  }
}
