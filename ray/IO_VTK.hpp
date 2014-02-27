/*
	(c) 2006 Ryan Dickie
	This is the VTK IO reader/writer
	It only does binary. Assumes big endian byte order for vtk file
	Tensors are not yet implemented.
	Hardly any error checking
*/

#include <sstream>
#include <exception>
#include <fstream>
#include <iostream>

#include "Volume3D.hpp"
#include "IO.hpp"


#ifndef IO_VTK_HPP
#define IO_VTK_HPP

namespace rysci
{
namespace IO
{
	namespace VTK
	{
		PointType findPointType( const std::string& a )
		{
			PointType b = UNKNOWN;

			if ( a == "SCALARS")
				b = SCALAR;
			else if ( a == "VECTORS")
				b = VECTOR;
			else if ( a == "TENSORS")
				b = VECTOR;		
			return b;
		}

		PrimType findPrimType ( const std::string& a )
		{
			PrimType b = VOID;
			
			if ( a == "unsigned_char" )
				b = UCHAR;
			else if ( a == "char" )
				b = CHAR;
			else if ( a == "unsigned_short")
				b = USHORT;
			else if ( a == "short" )
				b = SHORT;
			else if ( a == "unsigned_int" )
				b = UINT;
			else if ( a == "int" )
				b = INT;
			else if ( a == "unsigned_long" )
				b = ULONG;
			else if ( a == "long" )
				b = LONG;
			else if ( a == "float" )
				b = FLOAT;
			else if ( a == "double" )
				b = DOUBLE;

			return b;
		}

		typePair FromString ( const strPair& a )
		{
			typePair b;
			b.first = findPointType( a.first );
			b.second = findPrimType( a.second );
			return b;
		}

		strPair ToString( const typePair& a )
		{
			strPair b;
			switch ( a.first )
			{
				case SCALAR:
					b.first = "SCALARS";
					break;
				case VECTOR:
					b.first = "VECTORS";
					break;
				case TENSOR:
					b.first = "TENSORS";
					break;
				default:
					b.first = "";
			}
			switch ( a.second )
			{
				case UCHAR:
					b.second = "unsigned_char"; 
					break;
				case CHAR:
					b.second = "char";
					break;
				case USHORT :
					b.second = "unsigned_short";
					break;
				case SHORT:
					b.second = "short";
					break;
				case UINT:
					b.second = "unsigned_int";
					break;
				case INT:
					b.second = "int";
					break;
				case ULONG:
					b.second = "unsigned_long"; 
					break;
				case LONG:
					b.second = "long";
					break;
				case FLOAT:
					b.second = "float";
					break;
				case DOUBLE:
					b.second = "double";
					break;
				default:
					b.second = "";
			}	

			return b;
		}

	   	template<typename T>
		std::auto_ptr< Volume3D<T> > read(const std::string fileName)
		{
			typedef Volume3D<T> Vol3D;	
				
			//make sure type matches
			VolAttribs attrs;
			try{	
				std::ifstream in(fileName.c_str(), std::ios::binary);
			
				char header[50];
				in.getline(header,50); // # vtk version whatever
				in.getline(header,50); // dataset name
				in.getline(header,50); // dataset type
				if (in.peek() != 'D') //go back!
					in.getline(header,50); //whitespace
				in.getline(header,50); //dataset structured_points
	
				//TODO: deal with out of order ones... 
				std::string field;
				in>>field>>attrs.size[0]>>attrs.size[1]>>attrs.size[2];
				in>>field>>attrs.origin[0]>>attrs.origin[1]>>attrs.origin[2];
				in>>field>>attrs.spacing[0]>>attrs.spacing[1]>>attrs.spacing[2];			
				
				std::string pointData;
				int numPoints;
				in>>pointData>>numPoints;
				//check that its all good and matches
			
				strPair fileTypes_s;
				std::string setName;
				in>>fileTypes_s.first>>setName>>fileTypes_s.second;
				
				const T ass(0); //will give  uninitialized warning...	

				const typePair fileTypes(FromString(fileTypes_s));
				const typePair thisTypes(typeClass(ass));

				if (thisTypes != fileTypes) //todo look at case sensitivity
				{
					strPair t = ToString(thisTypes);
					strPair f = ToString(fileTypes);
					if ((thisTypes.second == CHAR && fileTypes.second==UCHAR) || (thisTypes.second == UCHAR && fileTypes.second ==CHAR))
					{
						std::cout<<"Warning casting "<<f.second<<" as "<<t.second<<std::endl;
					} else {
						std::cout<<"File Types: "<<f.first<<" "<<f.second<<std::endl;
						std::cout<<"Reader Types: "<<t.first<<" "<<t.second<<std::endl;
						throw std::exception();
					}
				}

				in.ignore();	
				if (fileTypes.first == SCALAR)
				{
					in.getline(header,50);
				}
		
				const int size = attrs.number_of_voxels();
				// well technically i didn't use reinterpret_cast :rolleyes:
				T* dat_T = static_cast<T*>(new T[size]);
	
				in.read( reinterpret_cast<char*>(dat_T) , size*sizeof(T) );
				in.close();
	
				//vtk binary files are in big-endian usually..
				if(!endianness::isBigEndian())
				{
					sizePair sz = sizeOf(thisTypes);
					endianness::swap(dat_T, dat_T + size, sz.second);
				}
				std::auto_ptr<Vol3D> v(new Vol3D(attrs,dat_T));
	
				return v;
	
			} catch (std::exception& e)
			{
				std::cerr<<"Error opening file: "<<fileName<<std::endl;
				throw e;
			}
		}

		template<typename T>
		void write( const std::string fileName, const Volume3D<T>& vol )
		{
			//typedef Volume3D<T>::Attribs Attribs;
			
			//writes the header
			std::ofstream file( fileName.c_str(), std::ios::trunc | std::ios::binary );

			//the header is in ascii, the rest is in binary
			std::stringstream header;
			header<<"# vtk DataFile Version 3.0\n";
			header<<"unnamed\n";
			header<<"BINARY"<<"\n";
			header<<"\n";	

			VolAttribs attrs = vol.attribs();
	
			header<<"DATASET STRUCTURED_POINTS\n";
			header<<"DIMENSIONS "<<attrs.size[0]<<" "<<attrs.size[1]<<" "<<attrs.size[2]<<"\n";
			header<<"ORIGIN "<<attrs.origin[0]<<" "<<attrs.origin[1]<<" "<<attrs.origin[2]<<"\n";	
			header<<"SPACING "<<attrs.spacing[0]<<" "<<attrs.spacing[1]<<" "<<attrs.spacing[2]<<"\n";
			header<<"\n";
		
			typePair thisTypes = typeClass(vol[0]);
			strPair thisTypes_s = ToString(thisTypes);
			header<<"POINT_DATA "<<static_cast<int>( vol.size() )<<"\n";
			header<<thisTypes_s.first<<" unnamed "<<thisTypes_s.second<<"\n";
			if (thisTypes.first == SCALAR)
				header<<"LOOKUP_TABLE default\n";

			file<<header.str();
			//now to actually write the damn data


			//vtk binary files are in big-endian usually..
			//pretty retarded how c++ iostreams requires const char*
			if(!endianness::isBigEndian())
			{
				Volume3D<T> v2(vol); //because its not proper to overwrite the original data!!!
				sizePair sz = sizeOf(thisTypes);
				endianness::swap(v2.begin(), v2.end(), sz.second);
				const char* dat_c = reinterpret_cast<const char*>(v2.begin());
				file.write(dat_c,v2.size_bytes()); 
			} else {
				const char* dat_c = reinterpret_cast<const char*>(vol.begin());
				file.write(dat_c,vol.size_bytes()); 
			}

			file.close();
		}

	} //end of namespace vtk
}//end of namespace io
} //end of namespace rysci

#endif
