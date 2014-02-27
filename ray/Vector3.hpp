/*
(c)2006 by Ryan Dickie
Designed to be a damn fast POD

This should be nice for compilers to auto-vectorize.
*/

#ifndef VECTOR3_HPP
#define VECTOR3_HPP

#include <cstddef>
#include <cassert>

namespace rysci
{
	template<class T>
	class Vector3
	{
		private:
			T dat[3];
	
		public:
			Vector3()
			{
				dat[0] = 0;
				dat[1] = 0;
				dat[2] = 0;
			}

			// int a(3) now has same syntax as Vector3<int> a(3);
			// also convenient for setting all 1 value..
			Vector3( T a )
			{
				dat[0] = a;
				dat[1] = a;
				dat[2] = a;
			}

			Vector3(const Vector3& copy)
			{
				dat[0] = copy[0];
				dat[1] = copy[1];
				dat[2] = copy[2];
			}
		
			//pass by value!
			Vector3(T x, T y, T z)
			{
				dat[0]=x;
				dat[1]=y;
				dat[2]=z;
			}


			inline T& operator[](std::size_t i)
			{
				assert( i <3 ); //i heart
				return dat[i];
			}

			
			inline const T& operator[](std::size_t i) const
			{
				assert( i <3 );	
				return dat[i];
			}

			void reset()
			{
				dat[0] = 0;
				dat[1] = 0;
				dat[2] = 0;
			}

			bool isZeroVector() const
			{
				return (dat[0] == 0 && dat[1] == 0 && dat[2] == 0);
			}
		
			inline T dotProduct(const Vector3& a) const
			{
				T retVal;
				retVal = a[0]*dat[0] + a[1]*dat[1] + a[2]*dat[2];
				return retVal;
			}

			inline T sum() const
			{
				return (dat[0] + dat[1] + dat[2]);
			}

			//if you want to store non prim-types in this vector
			//you will have to overload square root!, != 0
			inline T magnitude() const
			{
				T retVal;
				if (isZeroVector())
					return 0;
				else
					retVal = sqrt( dotProduct(*this) );
				return retVal;
			}
			
			inline void negate()
			{
				dat[0] = -dat[0];
				dat[1] = -dat[1];
				dat[2] = -dat[2];
			}

			inline void normalize()
			{
				T mag = magnitude();
				if (!mag) //expect this to be rare event...
					return;
				(*this) *= (1.0/mag);	
			}

			inline Vector3 normal() const
			{
				Vector3 v(*this);
				v.normalize();
				return v;
			}

			inline void clear()
			{
				dat[0] = 0;
				dat[1] = 0;
				dat[2] = 0;
			}

			inline void pow(double exp)
			{
				pow(dat[0],exp);
				pow(dat[1],exp);
				pow(dat[2],exp);
			}

			inline bool operator==(const Vector3& a ) const
			{
				return ( dat[0] == a[0] && dat[1] == a[1] && dat[2] == a[2] );
			}

			inline bool operator!=(const Vector3& a ) const
			{
				return ( dat[0] != a[0] || dat[1] != a[1] || dat[2] != a[2] );
			}

			inline void operator=(const Vector3& a )
			{
				dat[0] = a[0];
				dat[1] = a[1];
				dat[2] = a[2];
			}

			inline Vector3 operator+(const Vector3& a) const
			{
				Vector3 b;
				b[0] = dat[0] + a[0];
				b[1] = dat[1] + a[1];
				b[2] = dat[2] + a[2];
				return b;
			}
	
			inline Vector3 operator-(const Vector3& a) const
			{
				Vector3 b;

				b[0] = dat[0] - a[0];
				b[1] = dat[1] - a[1];
				b[2] = dat[2] - a[2];

				return b;
			}
		
			//negate operator...
			inline Vector3 operator-() const
			{
				Vector3 b;

				b[0] = -dat[0];
				b[1] = -dat[1];
				b[2] = -dat[2];

				return b;
			}

			inline double operator*(const Vector3& a) const
			{
				return dotProduct(a);
			}

			//this allows for scalar * This;
			//need another operator to define this*scalar;
			inline Vector3 operator*(const T a) const
			{
				Vector3 b;

				b[0] = dat[0] * a;
				b[1] = dat[1] * a;
				b[2] = dat[2] * a;

				return b;
			}

			inline Vector3 operator/(const T a) const
			{
				Vector3 b;

				b[0] = dat[0] / a;
				b[1] = dat[1] / a;
				b[2] = dat[2] / a;

				return b;
			}

			inline void operator+=(const Vector3& a)
			{
				dat[0]+=a[0];
				dat[1]+=a[1];
				dat[2]+=a[2];
			}

			inline void operator-=(const Vector3& a)
			{
				dat[0]-=a[0];
				dat[1]-=a[1];
				dat[2]-=a[2];
			}

			inline void operator*=(const Vector3& a)
			{
				dat[0]*=a[0];
				dat[1]*=a[1];
				dat[2]*=a[2];
			}

			inline void operator*=(const T a)
			{
				dat[0]*=a;
				dat[1]*=a;
				dat[2]*=a;
			}
			inline void operator/=(const T a)
			{
				dat[0]/=a;
				dat[1]/=a;
				dat[2]/=a;
			}		
	}; //end of class Vector3

} //end of namespace rysci

#endif
