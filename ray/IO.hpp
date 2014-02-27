/*
	(c) 2006 Ryan Dickie
	This class has general IO code. Can find out the type here and shit.
	It requires data structures like array which have a plain 'ol data
	memory layout. 
*/
//#include <boost/array.hpp>
#include <utility> //std::pair
#include <string>
#include "Tensor3.hpp"
#include "Endianness.hpp"

#ifndef IO_HPP
#define IO_HPP

namespace rysci
{
	//wish c++ enums worked like c# enums..
	enum PrimType { VOID = 0, UCHAR, CHAR, USHORT, SHORT, UINT, INT, ULONG, LONG, FLOAT, DOUBLE };
	enum PointType { UNKNOWN = 100, SCALAR, VECTOR, TENSOR };

	typedef std::pair<PointType, PrimType> typePair;
	typedef std::pair<std::string,std::string> strPair;
	typedef std::pair< size_t, size_t > sizePair;
		

namespace IO
{
	/*
		typeClass finds the types for the volume at compile time using
		overloading. It's a crude hack around a missing language feature.
	*/
	typePair typeClass ( const Vector3<unsigned char>& a ){ return typePair(VECTOR,UCHAR); }
	typePair typeClass ( const Vector3<char>& a ){ return typePair(VECTOR,CHAR); }
	typePair typeClass ( const Vector3<unsigned short>& a ){ return typePair(VECTOR,USHORT); }
	typePair typeClass ( const Vector3<short>& a ){ return typePair(VECTOR,SHORT); }
	typePair typeClass ( const Vector3<unsigned int>& a ){return typePair(VECTOR,UINT); }
	typePair typeClass ( const Vector3<int>& a ){return typePair(VECTOR,INT); }
	typePair typeClass ( const Vector3<unsigned long>& a ){return typePair(VECTOR,ULONG); }
	typePair typeClass ( const Vector3<long>& a ){return typePair(VECTOR,LONG); }
	typePair typeClass ( const Vector3<float>& a ){return typePair(VECTOR,FLOAT); }
	typePair typeClass ( const Vector3<double>& a ){return typePair(VECTOR,DOUBLE); }


	typePair typeClass ( const Tensor3<unsigned char>& a ){ return typePair(TENSOR,UCHAR); }
	typePair typeClass ( const Tensor3<char>& a ){ return typePair(TENSOR,CHAR); }
	typePair typeClass ( const Tensor3<unsigned short>& a ){ return typePair(TENSOR,USHORT); }
	typePair typeClass ( const Tensor3<short>& a ){ return typePair(TENSOR,SHORT); }
	typePair typeClass ( const Tensor3<unsigned int>& a ){return typePair(TENSOR,UINT); }
	typePair typeClass ( const Tensor3<int>& a ){return typePair(TENSOR,INT); }
	typePair typeClass ( const Tensor3<unsigned long>& a ){return typePair(TENSOR,ULONG); }
	typePair typeClass ( const Tensor3<long>& a ){return typePair(TENSOR,LONG); }
	typePair typeClass ( const Tensor3<float>& a ){return typePair(TENSOR,FLOAT); }
	typePair typeClass ( const Tensor3<double>& a ){return typePair(TENSOR,DOUBLE); }



	typePair typeClass ( unsigned char a ){ return typePair(SCALAR,UCHAR); }
	typePair typeClass ( char a ){ return typePair(SCALAR,CHAR); }
	typePair typeClass ( unsigned short a ){ return typePair(SCALAR,USHORT); }
	typePair typeClass ( short a ){ return typePair(SCALAR,SHORT); }
	typePair typeClass ( unsigned int a ){return typePair(SCALAR,UINT); }
	typePair typeClass ( int a ){return typePair(SCALAR,INT); }
	typePair typeClass ( unsigned long a ){return typePair(SCALAR,ULONG); }
	typePair typeClass ( long a ){return typePair(SCALAR,LONG); }
	typePair typeClass ( float a ){return typePair(SCALAR,FLOAT); }
	typePair typeClass ( double a ){return typePair(SCALAR,DOUBLE); }

	sizePair sizeOf(const typePair& a)
	{
		sizePair b(1,1);

		if( a.first == VECTOR )
			b.first = 3;
		else if (a.first == TENSOR)
			b.first = 9;
		else //scalar and other
			b.first = 1;

		switch ( a.second )
		{
			case VOID:
				b.second = 0;
				break;
			case UCHAR:
			case CHAR:
				b.second = 1;
				break;
			case USHORT:
			case SHORT:
				b.second = 2;
				break;
			case UINT:
			case INT:
			case FLOAT:
				b.second = 4;
				break;	
			case ULONG:
			case LONG:
			case DOUBLE:
				b.second = 8;
				break;
			default:
				b.second = 1;
				break;
		}
		return b;
	}
} //end of namespace io
} //end of namespace rysci

#endif
