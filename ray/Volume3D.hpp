/*
 (c) 2006 by Ryan Dickie
	Contains a 3D volume class and an attributes class for the volume.
*/

#include "Vector3.hpp"

#ifndef VOLUME_HPP
#define VOLUME_HPP

#include <Magick++.h> 

namespace rysci
{
	typedef Vector3<double> Point;
	typedef ssize_t FlatIndex; //for direct access to memory.
	typedef Vector3<FlatIndex> ArrayIndex; //this is because of roll-over


	struct VolAttribs
	{
		ArrayIndex size; //just because it has overloaded operators.
		Point spacing; //.. not so much diff from a regular array otherwise
		Point origin;

		//maybe add a tostring method.
		
		//initializes all elements to 0 
		VolAttribs()
		{
			size[0] = size[1] = size[2] = 0;
			spacing[0] = spacing[1] = spacing[2] = 0;
			origin[0] = origin[1] = origin[2] = 0;
		}

		VolAttribs(const VolAttribs& a)
		{
			size = a.size;
			spacing = a.spacing;
			origin = a.origin;
		}

		bool operator==(const VolAttribs& a) const
		{
			return ( size == a.size && spacing == a.spacing && origin == a.origin);
		}

		bool operator!=(const VolAttribs& a) const
		{
			return ( size != a.size || spacing != a.spacing || origin != a.origin);
		}

		bool operator=(const VolAttribs& a)
		{
			size = a.size;
			origin = a.origin;
			spacing = a.spacing;

			return true;
		}	

		FlatIndex number_of_voxels() const
		{
			return ( size[0] * size[1] * size[2] );
		}
	}; //end of arrayAttribs

	template<typename T>
	class Volume3D
	{
		/**
		 * look into way of interpolating from point. 
		 * add a bunch of asserts and shit
		 *
		 * Doesn't deal for case when volume has 0 size!!
		 */
	    private:
		T* dat;
		VolAttribs attrs;

	    public:

		//sizes better match!
		Volume3D(const VolAttribs& a, T* d)
		{
			attrs = a;
			dat = d;
		}

		Volume3D(const VolAttribs& a)
		{
			attrs = a;
			dat = new T[attrs.number_of_voxels()];
			clear_voxels();
		}

		//create a simple array with this number of voxels
		//does not explicitly clear voxels.
		Volume3D(const ArrayIndex& extents)
		{
			//create an array with these dimensions..
			attrs.size = extents;
			attrs.origin[0] = attrs.origin[1] = attrs.origin[2] = 0;
			attrs.spacing[0] = attrs.spacing[1] = attrs.spacing[2] = 1.0;

			dat = new T[attrs.number_of_voxels()];
			//clear_voxels();
		}

		Volume3D(const Volume3D& v)
		{
			attrs = v.attrs;
			dat = new T[attrs.number_of_voxels()];
			memcpy(dat, v.data(), size_bytes());
		}

		~Volume3D()
		{
			delete [] dat;
		}

		void setOrigin(const Point& org)
		{
			attrs.origin = org;
		}

		void setSpacing(const Point& spacing)
		{
			attrs.spacing = spacing;
		}

		//raw pointer to data. useful for writting to file or something
		//used in conjunction with size_bytes.
		//if you need write access use operator[](FlatIndex i)
		const T* data() const
		{
			return dat;
		}

	
		//returns a copy of the attributes
		VolAttribs copy_attributes() const
		{
			return attrs;
		}
		
		//returns the reference to the attributes.
		const VolAttribs& attribs() const
		{
			return attrs;
		}

		const T* begin() const
		{
			return dat;
		}
	
		//returns pointer to last item in the array
		const T* end() const
		{
			return dat + size();
		}

		inline T* begin()
		{
			return dat;
		}

		inline T* end()
		{
			return dat + size();
		}


		//only calculated on first call.
		inline FlatIndex size() const
		{
			return attrs.number_of_voxels();
		}

		inline FlatIndex size_bytes() const
		{
			return sizeof(T)*size();
		}

		//if this is a common operation be sure to make number_of_voxels and size_bytes
		//a constant somewhere..
		void clear_voxels()
		{
			std::fill(begin(), end(), 0);
		}

		inline Point getPoint(const ArrayIndex& i) const
		{
			Point p;

			p[0] = i[0]*attrs.spacing[0] + attrs.origin[0];
			p[1] = i[1]*attrs.spacing[1] + attrs.origin[1];
			p[2] = i[2]*attrs.spacing[2] + attrs.origin[2];
			
			return p;
		}

		inline ArrayIndex getIndex(const Point& p) const
		{
			ArrayIndex index;

                	index[0] = static_cast<FlatIndex> ( round( ( p[0] - attrs.origin[0] ) / attrs.spacing[0] ) );
			index[1] = static_cast<FlatIndex> ( round( ( p[1] - attrs.origin[1] ) / attrs.spacing[1] ) );
			index[2] = static_cast<FlatIndex> ( round( ( p[2] - attrs.origin[2] ) / attrs.spacing[2] ) );

		        return index;
		}


		//simple check to see if the index ever goes past positive bounds
		// (index is bounded on lower side by 0, since FlatIndex is unsigned)
		// 0 is always a valid index.
		inline bool isValidIndex(const ArrayIndex& i) const
		{
			if ( i[0]>= attrs.size[0] || i[0] < 0 )
					return false;
			if ( i[1]>= attrs.size[1] || i[1] < 0 )
					return false;
			if ( i[2]>= attrs.size[2] || i[2] < 0 )
					return false;
			return true;
		}

		inline bool isValidIndex( FlatIndex i ) const
		{
			return ( i < size() && i >= 0 );
		}

		inline ArrayIndex flatToArrayIndex( FlatIndex i ) const
		{
			assert (isValidIndex(i));

			static const FlatIndex widthHeight(attrs.size[0]*attrs.size[1]); //one time cost!
			static const FlatIndex width(attrs.size[0]);

			//todo: find faster algorithm without division or modulo...
			ArrayIndex a;
			a[2] = i / widthHeight;
			a[1] = (i - a[2]*widthHeight) / width;
			a[0] = i - a[2]*widthHeight - a[1]*width;

			return a;
		}

		inline FlatIndex arrayToFlatIndex(const ArrayIndex& index) const
		{
			assert(isValidIndex(index));
			static const FlatIndex widthHeight(attrs.size[0]*attrs.size[1]); //one time cost!
			static const FlatIndex width(attrs.size[0]);
			return ( index[2]*widthHeight + index[1]*width + index[0] );
		}

		T& operator[](const ArrayIndex& index)
		{
			assert(isValidIndex(index));
			return dat[ arrayToFlatIndex(index) ]; //should be plenty fast..
		}

		const T& operator[](const ArrayIndex& index) const
		{
			assert(isValidIndex(index));
			return dat[ arrayToFlatIndex(index) ];
		}

		T& operator[](const FlatIndex& i)
		{
			assert( isValidIndex(i) );
			return dat[i];
		}

		const T& operator[](const FlatIndex& i) const
		{
			assert( isValidIndex(i) );
			return dat[i];
		}

		//won't check for different sizes..
		inline Volume3D& operator-(const Volume3D& v1) const
		{
			Volume3D v(v1.attrs());
			std::transform( begin(), end(), v1.begin(), v.begin(), std::minus<T>() );
			return v;
		}
		
		//the copy negate operator
		inline Volume3D& operator-()
		{
			Volume3D v(attrs());
			std::transform( begin(), end(), v.begin(), std::negate<T>() );
			return v;
		}

		inline Volume3D& operator+(const Volume3D& v1)
		{
			Volume3D v(v1.attrs());
			std::transform( begin(), end(), v1.begin(), v.begin(), std::plus<T>() );
			return v;
		}
	
		inline void operator+=(const Volume3D& v1)
		{
			std::transform( begin(), end(), v1.begin(), begin(), std::plus<T>() );
		}

		inline void operator-=(const Volume3D& v1)
		{
			std::transform( begin(), end(), v1.begin(), begin(), std::minus<T>() );
		}


		//if T is a vector then... 
		inline void operator*=(float scalar)
		{
			for ( T* i = begin(); i != end(); ++i )
				*i *= scalar;
		}

		//used in conjunction with incrIndex (also checks for beyond end if going through in x,y,z way.)
		inline bool isNotPastEnd(const ArrayIndex& index) const
		{
			return ( index[2] < attrs.size[2] ); 
		}

		//provides a way of increment the index
		/**
		 * for ( Index i = {0,0,0}; !isEnd(i); incrIndex(i) )
		 * {
		 *  	volume[i] = 42
		 * }
		 * 
		 * note: doesn't check if goes beyond end.
		 */
		inline void incrIndex( ArrayIndex& index ) const
		{
			index[0]++;
			if ( index[0] >= attrs.size[0] )
			{
				index[0] = 0;
				index[1]++;
				if (index[1] >= attrs.size[1])
				{
					index[1] = 0;
					index[2]++;
				}
			}
		}

		//ImageMagick wouldn't take "K" pixel type and then write file. 
		//So i'm having to RGB and then grayscale it. The API for it
		//needs work. I can't figure it out quick enough so bleh!.
		//CImg was simpler but CImg used ImageMagic for grayscale
		//writting... that is to say the command line app.. AND
		//the pipe action killed any possible concurrency so I have to switch
		void saveZSlice(std::string fileName, const size_t slice) const
		{
			const unsigned int width = static_cast<unsigned int>(attribs().size[0]);
			const unsigned int height = static_cast<unsigned int>(attribs().size[1]);
		
			assert ( slice < attribs().size[2] );
	
			Vector3<unsigned char>* pixels = new Vector3<unsigned char>[width * height];
			Vector3<unsigned char>* p = pixels;
			ArrayIndex a(0,0,slice);
			for (unsigned int y=0;y<height;y++)
			{
				for (unsigned int x=0;x<width;x++)
				{
					a[0] = x;
					a[1] = y;	
					*p++ = Vector3<unsigned char>((*this)[a]);
				}
			}
			Magick::Image i(width,height,"RGB", Magick::CharPixel, pixels);
   			i.type( Magick::GrayscaleType );
			i.write(fileName);
			delete [] pixels;
		}

		void saveYSlice(std::string fileName, const size_t slice)  const
		{
			const unsigned int width = static_cast<unsigned int>(attribs().size[0]);
			const unsigned int height = static_cast<unsigned int>(attribs().size[2]);
			
			assert ( slice < attribs().size[1] );

			Vector3<unsigned char>* pixels = new Vector3<unsigned char>[width * height];
			Vector3<unsigned char>* p = pixels;
			ArrayIndex a(0,slice,0);
			for (unsigned int y=0;y<height;y++)
			{
				for (unsigned int x=0;x<width;x++)
				{
					a[0] = x;
					a[2] = y;	
					*p++ = Vector3<unsigned char>((*this)[a]);
				}
			}
			Magick::Image i(width,height,"RGB", Magick::CharPixel, pixels);
   			i.type( Magick::GrayscaleType );
			i.write(fileName);
			delete [] pixels;
		}

		void saveXSlice(std::string fileName, const size_t slice)  const
		{
			
			const unsigned int width = static_cast<unsigned int>(attribs().size[1]);
			const unsigned int height = static_cast<unsigned int>(attribs().size[2]);
			
			assert ( slice < attribs().size[0] );

			Vector3<unsigned char>* pixels = new Vector3<unsigned char>[width * height];
			Vector3<unsigned char>* p = pixels;
			ArrayIndex a(slice,0,0);
			for (unsigned int y=0;y<height;y++)
			{
				for (unsigned int x=0;x<width;x++)
				{
					a[1] = x;
					a[2] = y;
					*p++ = Vector3<unsigned char>((*this)[a]);
				}
			}
			Magick::Image i(width,height,"RGB", Magick::CharPixel, pixels);
   			i.type( Magick::GrayscaleType );
			i.write(fileName);
			delete [] pixels;
		}

		void saveZSliceRGB(std::string fileName, const size_t slice) const
		{
			const unsigned int width = static_cast<unsigned int>(attribs().size[0]);
			const unsigned int height = static_cast<unsigned int>(attribs().size[1]);
		
			assert ( slice < attribs().size[2] );
	
			Vector3<unsigned char>* pixels = new Vector3<unsigned char>[width * height];
			Vector3<unsigned char>* p = pixels;
			ArrayIndex a(0,0,slice);
			for (unsigned int y=0;y<height;y++)
			{
				for (unsigned int x=0;x<width;x++)
				{
					a[0] = x;
					a[1] = y;	
					*p++ = (*this)[a];
				}
			}
			Magick::Image i(width,height,"RGB", Magick::CharPixel, pixels);
			i.write(fileName);
			delete [] pixels;
		}

		void saveYSliceRGB(std::string fileName, const size_t slice)  const
		{
			const unsigned int width = static_cast<unsigned int>(attribs().size[0]);
			const unsigned int height = static_cast<unsigned int>(attribs().size[2]);
			
			assert ( slice < attribs().size[1] );

			Vector3<unsigned char>* pixels = new Vector3<unsigned char>[width * height];
			Vector3<unsigned char>* p = pixels;
			ArrayIndex a(0,slice,0);
			for (unsigned int y=0;y<height;y++)
			{
				for (unsigned int x=0;x<width;x++)
				{
					a[0] = x;
					a[2] = y;	
					*p++ = (*this)[a];
				}
			}
			Magick::Image i(width,height,"RGB", Magick::CharPixel, pixels);
			i.write(fileName);
			delete [] pixels;
		}

		void saveXSliceRGB(std::string fileName, const size_t slice)  const
		{
			
			const unsigned int width = static_cast<unsigned int>(attribs().size[1]);
			const unsigned int height = static_cast<unsigned int>(attribs().size[2]);
			
			assert ( slice < attribs().size[0] );

			Vector3<unsigned char>* pixels = new Vector3<unsigned char>[width * height];
			Vector3<unsigned char>* p = pixels;			
			ArrayIndex a(slice,0,0);
			for (unsigned int y=0;y<height;y++)
			{
				for (unsigned int x=0;x<width;x++)
				{
					a[1] = x;
					a[2] = y;
					*p++ = (*this)[a];
				}
			}
			Magick::Image i(width,height,"RGB", Magick::CharPixel, pixels);
			i.write(fileName);
			delete [] pixels;
		}

	};	//end of the class
}//end of namespace rysci

#endif
