/*
(c)2006 by Ryan Dickie
Designed to be a damn fast POD

This should be nice for compilers to auto-vectorize.
*/

#ifndef TENSOR3_HPP
#define TENSOR3_HPP

#include <cstddef>
#include <cassert>



namespace rysci
{
	template<class T>
	class Tensor3
	{

		private:
			T dat[9];
	
		public:
			Tensor3()
			{
				for (size_t i=0;i<9;i++)
					dat[i] = 0;
			}

			// int a(3) now has same syntax as Tensor3<int> a(3);
			// also convenient for setting all 1 value..
			Tensor3( T a )
			{
				for (size_t i=0;i<9;i++)
					dat[i] = a;
			}

			Tensor3(const Tensor3& copy)
			{
				for (size_t i=0;i<9;i++)
					dat[i] = copy[i];
			}
		
			//pass by value!
			Tensor3( const matrix<T>& t )
			{
				assert ( t.size1() = 3 );
				assert ( t.size2() = 3 );

				for (size_t i=0;i<3;i++)
					for (size_t j=0;j<3;j++)
						dat[i*3+j] = t(i,j);
			}

			inline double trace() const
			{
				return dat[0] + dat[4] + dat[8];

			}
			
			const inline T& operator() (size_t i, size_t j) const
			{
					assert( i<3 && j<3 );
					return dat[i*3+j];
			}

			inline T& operator() (size_t i, size_t j)
			{
					assert( i<3 && j<3 );
					return dat[i*3+j];
			}

			inline T& operator[](std::size_t i)
			{
				assert( i < 9 ); //i heart
				return dat[i];
			}

			
			inline const T& operator[](std::size_t i) const
			{
				assert( i < 9 );	
				return dat[i];
			}

			void clear()
			{
				for (size_t i=0; i<9; i++)
					dat[i] = 0;
			}

			bool isZeroTensor() const
			{
				for (size_t i=0;i<9;i++)
					if (dat[i] != 0) return false;
				return true;
			}
		
			//be warned inverting makes the transpose
			void transpose()
			{
				double b = dat[1];
				double c = dat[2];
				double f = dat[5];
				
				dat[1] = dat[3];
				dat[2] = dat[6];
				dat[5] = dat[7];

				dat[3] = b;
				dat[6] = c;
				dat[7] = f;
			}

			inline bool operator==(const Tensor3& a ) const
			{
				for (size_t i =0;i<9;i++)
					if (dat[i] != a[i]) return false;
				return true;
			}

			inline bool operator!=(const Tensor3& a ) const
			{
				for (size_t i =0;i<9;i++)
					if (dat[i] != a[i]) return true;
				return false;			
			}

			inline void operator=(const Tensor3& a )
			{
				for (size_t i=0;i<9;i++)
					dat[i] = a[i];
			}


			inline Tensor3 operator*(const Tensor3& inp) const
			{
				Tensor3 t;
				double a,b,c,d,e,f,g,h,i;
				a = dat[0]; b = dat[1]; c = dat[2]; d = dat[3]; e = dat[4]; f = dat[5]; g = dat[6]; h = dat[7]; i = dat[8];
				double j,k,l,m,n,o,p,q,r;
				j = inp[0]; k = inp[1]; l = inp[2]; m = inp[3]; n = inp[4]; o = inp[5]; p = inp[6]; q = inp[7]; r = inp[8];

				t[0] = a*j + b*m + c*p;
				t[1] = a*k + b*n + c*q;
				t[2] = a*l + b*o + c*r;

				t[3] = d*j + e*m + f*p;
				t[4] = d*k + e*n + f*q;
				t[5] = d*l + e*o + f*r;

				t[6] = g*j + h*m + i*p;
				t[7] = g*k + h*n + i*q;
				t[8] = g*l + h*o + i*r;

/*
			for symmetric
				t[0] = g*a + h*b + i*c;
				t[1] = g*b + h*d + i*e;
				t[2] = g*c + h*e + i*f;
				t[3] = h*a + j*b + k*c;
				t[4] = h*b + j*d + k*e;
				t[5] = h*c + j*e + k*f;
				t[6] = i*a + k*b + l*c;
				t[7] = i*b + k*d + l*e;
				t[8] = i*c + k*e + l*f;
*/
				return t;
			}	

			inline Tensor3 operator+(const Tensor3& a) const
			{
				Tensor3 c;
				for (size_t i=0;i<9;i++)
					c[i] = a[i] + dat[i];

				return c;
			}

			inline Tensor3 operator-(const Tensor3& a) const
			{
				Tensor3 c;
				for (size_t i=0;i<9;i++)
					c[i] = a[i] - dat[i];
				return c;
			}

			inline T determinant() const
			{
				double a = dat[0]; double b = dat[1]; double c = dat[2];
				double d = dat[3]; double e = dat[4]; double f = dat[5];
				double g = dat[6]; double h = dat[7]; double i = dat[8];
					
				T one = a*(e*i - f*h);
				T two = d*(c*h - b*i);
				T three = g*(b*f - c*e);
				
				return (one+two+three);
			}
 /* Matrix inversion routine.
        Since its only 3x3 matrix i can not have to worry about growth of operations
        my goal is to minimize multiplications and divisions. 
        must be symmetric
    */
	Tensor3 inverse() const
	{
		Tensor3 inv;
		double a = dat[0]; double b = dat[1]; double c = dat[2];
		double d = dat[3]; double e = dat[4]; double f = dat[5];
		double g = dat[6]; double h = dat[7]; double i = dat[8];

		T det = determinant();
		inv[0] = e*i - f*h;
		inv[1] = c*h - b*i;
		inv[2] = b*f - c*e;
		inv[3] = f*g - d*i;
		inv[4] = a*i - c*g;
		inv[5] = c*d - a*f;
		inv[6] = d*h - e*g;
		inv[7] = b*g - a*h;
		inv[8] = a*e - b*d;
		for (size_t i=0;i<9;i++)
			inv[i] /= det;

	        return inv;
	}

	Tensor3 symmetric_inverse() const
	{
		Tensor3 inv;
		double a = dat[0]; double b = dat[1]; double c = dat[2];
		   		   double e = dat[4]; double f = dat[5];
						      double i = dat[8];

		T det = determinant();
		inv[0] = e*i - f*f;
		inv[1] = c*f - b*i;
		inv[2] = b*f - c*e;
		inv[3] = inv[1];
		inv[4] = a*i - c*c;
		inv[5] = c*b - a*f;
		inv[6] = inv[2];
		inv[7] = inv[5];
		inv[8] = a*e - b*b;
		for (size_t i=0;i<9;i++)
			inv[i] /= det;

	        return inv;
	}

	}; //end of class Tensor3

} //end of namespace rysci

#endif
