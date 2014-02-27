/*
	(c) 2006 by Ryan Dickie
	I can't believe endianess isn't part of any standard c++ libraries.
	There are those for short and int but not long. 
	I started a debate about it in the c++ usenet standard group. :D
	This is just a crude hack. Probably not even that fast...
    but it is portable to IA64 and x86
*/

#include <stdint.h>


#ifndef ENDIANNESS_HPP
#define ENDIANNESS_HPP

namespace rysci
{
namespace endianness
{
	union Long
	{
		uint64_t ul;
		uint8_t uc[8];
	};
			
	union Int
	{
		uint32_t ui;
		uint8_t uc[4];
	};

	union Short
	{
		uint16_t us;
		uint8_t uc[2];
	};

	/*
		Converting endianess is about 25% of the read/write time
		on a volume. 
		For my thesis Long swap is the only one being used.
	*/	
	inline void swap64(Long& a)
	{
		if ( a.ul == 0) //saves pipeline.
			return;

		Long copy = a;
		a.uc[0] = copy.uc[7];
		a.uc[1] = copy.uc[6];
		a.uc[2] = copy.uc[5];
		a.uc[3] = copy.uc[4];
		a.uc[4] = copy.uc[3];
		a.uc[5] = copy.uc[2];
		a.uc[6] = copy.uc[1];
		a.uc[7] = copy.uc[0];
	}
	
	inline void swap32(Int& a)
	{
		Int copy = a;
		a.uc[0] = copy.uc[3];
		a.uc[1] = copy.uc[2];
		a.uc[2] = copy.uc[1];
		a.uc[3] = copy.uc[0];
	}

	inline void swap16(Short& a)
	{
		Short copy = a;
		a.uc[0] = copy.uc[1];
		a.uc[1] = copy.uc[0];
	}

	inline bool isBigEndian()
	{
		Short a;
		a.us = 1;
		if (a.uc[0])
			return false;
		return true;
	}

	template <typename T>
	void swap( T* begin, T* end, size_t sizeElem )
	{
		switch ( sizeElem )
		{
			case 2:
				std::for_each(reinterpret_cast<Short*>(begin),reinterpret_cast<Short*>(end),swap16);
				break;
			case 4:
				std::for_each(reinterpret_cast<Int*>(begin),reinterpret_cast<Int*>(end),swap32);
				break;
			case 8:
				std::for_each(reinterpret_cast<Long*>(begin),reinterpret_cast<Long*>(end),swap64);
				break;
		}
	}

} //end of namespace endianness
} //end of namespace rysci

#endif
