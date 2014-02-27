/*
 (c) 2006 by Ryan Dickie
*/
#ifndef UTIL_HPP
#define UTIL_HPP

#include <boost/config.hpp>
#include <boost/random.hpp>
#include <boost/progress.hpp>

namespace rysci
{

	//calling randomNumber() produces a random number between 0 and 1.
	boost::lagged_fibonacci607 rng;
	boost::uniform_real<> xrand(0,1.0);
	boost::variate_generator<boost::lagged_fibonacci607 &, boost::uniform_real<> > randomNumber(rng, xrand);

	//this method is not designed for performance...
	template<class T>
	std::string printVector(const Vector3<T>& v)
	{
		std::stringstream ret;
		ret<<" ( "<<v[0]<<", "<<v[1]<<", "<<v[2]<<" ) ";
		return ret.str();
	}

	std::string printAttrs(const VolAttribs& a)
	{
		std::string ret;
		
		ret = "Origin: " + printVector(a.origin) + "\n";
		ret += "Spacing: " + printVector(a.spacing) + "\n";
		ret += "Size: " + printVector(a.size) + "\n";

		return ret;
	}


	template<class T>
	inline Vector3<T> crossProduct( const Vector3<T>& a, const Vector3<T>& b)
	{
		Vector3<T> c; //fine to leave uninitialized
	
		c[0] = a[1]*b[2] - a[2]*b[1];
		c[1] = a[2]*b[0] - a[0]*b[2];
		c[2] = a[0]*b[1] - a[1]*b[0];

		return c;
	}


	double findAngle(const Vector3<double>& a, const Vector3<double>& b)
	{
		double dp = a.dotProduct(b);
	
		dp/= (a.magnitude() * b.magnitude());
		if( dp >= 1.0) dp = 1.0; //deals with rounding errors
		if ( dp <= -1.0) dp = -1.0;
	
		return acos(dp);
	}

	float findAngle(const Vector3<float>& a, const Vector3<float>& b)
	{
		float dp = a.dotProduct(b);
	
		dp/= (a.magnitude() * b.magnitude());
		if ( dp >= 1.0) dp = 1.0; //deals with rounding errors
		if ( dp <= -1.0) dp = -1.0;
	
		return acos(dp);
	}


	inline float degToRad(float deg)
	{
		return (deg * 0.017453293);
	}

	inline double degToRad(double deg)
	{
		return (deg * 0.017453293);
	}

	inline float radToDeg(float rad)
	{
		return (57.29577951 * rad);
	}

	inline double radToDeg(double rad)
	{
		return (57.29577951 * rad); //ratio is 180/pi
	}

	template<class T>
	inline T calc_theta( Vector3<T> &v )
	{
	        return atan(v[1] / v[0]);
	}

	template<class T>
	inline T calc_phi ( Vector3<T> &v )
	{
		return acos( v[2] / v.magnitude() );
	}

	//a, b
	template<class T>
	inline T findAngle(const Vector3<T>& a, const Vector3<T>& b)
	{
		T dp = a.normal() * b.normal();
		if ( dp >= 1.0) dp = 1; //deals with rounding errors
		if ( dp <= -1.0) dp = -1;
		return acos(dp);
	}

	template< typename T >
	inline void normalizeVolume( Volume3D< Vector3<T> >& vol)
	{
		std::for_each(vol.begin(), vol.end(), std::mem_fun_ref(&Vector3<T>::normalize));
	}

	/* some typeical types and colors */	
	typedef Vector3<unsigned char> RGB;
	typedef Volume3D<RGB> RGBVolume;
	typedef Volume3D<unsigned char> Mask;
	typedef Vector3<double> Vec3D;
	typedef Volume3D<Vec3D> Vec3DVolume;

	const RGB Red(255,0,0);
	const RGB Green(0,255,0);
	const RGB Blue(0,0,255);
	const RGB White(255,255,255);
	const RGB Black(0,0,0);



}
#endif
