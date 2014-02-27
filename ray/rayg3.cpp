/*
 * Works about 30 fps right now. That's just simple ray-casting at 400x400
 * I haven't added light rays or anything. I suppose a lot of overhead comes
 * from inefficient use of my thesis code volume.
 *
 */
#include <allegro.h>
#include "Universal.hpp"
using namespace rysci;
using namespace std;

typedef Vector3<double> Vec;
typedef Vector3<unsigned char> Colour;
typedef Vector3<double> ColourF;
typedef Volume3D<Colour> Screen; //promote code-reuse for the moment!

auto_ptr< Mask > inputVol;

//some standard colours
const ColourF red(1.0,0,0);
const ColourF green(0,1.0,0);
const ColourF blue(0,0,1.0);
const ColourF yellow(1.0,1.0,0);
const ColourF magenta(1.0,0,1.0);
const ColourF cyan(0,1.0,1.0);
const ColourF black(0,0,0);
const ColourF dgray(.25,.25,.25);
const ColourF mgray(.5,.5,.5);
const ColourF lgray(.75,.75,.75);
const ColourF white(1.0,1.0,1.0);

inline double square(double a)
{
	return a*a;
}

struct Ray{
	Vec org;
	Vec dir;
	int ttl; //time to live.. ie: how many bounces remaining
	Ray(Vec& o, Vec& d):org(o),dir(d),ttl(20){};
	Ray():ttl(20){};
};
typedef std::vector<Ray> RL;


const int DefaultTTL = 20; //how many possible bounces for reflection etc.

//d-solve for dirty solve. it does pivit though.
//	[ a b | x]
//	[ c d | y]
//	[ e f | z]
//input params form the column Vecs
Vec dsolve2( const Vec& d1, const Vec& d2, const Vec& p )
{
	//a cannot be 0! see s calculations below
	
	double a = d1[0]; double b = d2[0]; double x = p[0];
	double c = d1[1]; double d = d2[1]; double y = p[1];
	double e = d1[2]; double f = d2[1]; double z = p[2];

	if (a == 0)
	{ //so i swap rows. 
		std::swap(a,c);
		std::swap(b,d);
		std::swap(x,y);			
	}		

	double t = (y*a - x*c) / (d*a - b*c);
	double s = (x - t * b) / a;

	//s is returned negative to avoid a sign flip going in!
	Vec ret(-s,t,0);
	return ret;
}

// 3x3 matrix inverse (in place!)
//	[ a b c]
//	[ d e f]
//	[ g h i]
bool invert ( Vec& d1, Vec& d2, Vec& d3)
{
	const double a = d1[0]; const double d = d1[1]; const double g = d1[2];
	const double b = d2[0]; const double e = d2[1]; const double h = d2[2];
	const double c = d3[0]; const double f = d3[1]; const double i = d3[2];

	const double ei = e*i; const double fh = f*h; const double fg = f*g;
	const double di = d*i; const double dh = d*h; const double ge = g*e;
	const double hc = h*c; const double ib = i*b; const double ai = a*i;
	const double gc = g*c; const double ah = a*h; const double bf = b*f;
	const double ce = c*e; const double dc = d*c; const double af = a*f;
	const double ae = a*e; const double db = d*b; const double gb = g*b;

	const double det = (a * (ei - fh) + b * (fg - di) + c*(dh - fg));
	if (det == 0) return false;
	//1 over determinant
	const double deti = 1.0 / det;

	d1[0] = ei - fh; d1[1] = fg - di; d1[2] = dh - ge;
	d1 *= deti;
	d2[0] = hc - ib; d2[1] = ai - gc; d2[2] = gb - ah;
	d2 *= deti;
	d3[0] = bf - ce; d3[1] = dc - af; d3[2] = ae - db;
	d3 *= deti;
	return true;
}

// 3x3 matrix times Vec
// Ax = b
Vec linearTransform(const Vec& A1, const Vec& A2, const Vec& A3, const Vec& x)
{
	Vec b;
	b[0] = A1[0] * x[0] + A2[0] * x[1] + A3[0] * x[2];
	b[1] = A1[1] * x[0] + A2[1] * x[1] + A3[1] * x[2];
	b[2] = A1[2] * x[0] + A2[2] * x[1] + A3[2] * x[2];
	return b;
}

struct Light{
	Vec org;
	ColourF colour;

	Light(){};	

	Light(const Vec& origin, const ColourF& couleur):org(origin),colour(couleur){}
};


typedef std::vector<Light> LV;

class Material
{
  public:
	double diffuse;
	double reflect;
	double specular;
	double opacity;
	ColourF colour; //todo: texture!

	Material(double d, double s, double r, double o, const ColourF& c):diffuse(d),specular(s),reflect(r),opacity(o),colour(c){}
	Material()
	{
		diffuse = 0; specular, reflect = 0; opacity = 0; colour = black;
	}
};

class Prim
{
  public:
	Vec org;
	size_t id;
	virtual Vec getNormal(const Vec& ip) const = 0;//find the normal at the intersection point
	virtual int intersect( const Ray& ray, Vec& ip) const = 0;
	Material material;
};

class Sphere: public Prim
{
  public:
	double radius;
	Vec getNormal(const Vec& ip) const
	{
		return (ip-org) / radius;
	}
	//ip is intersection point
	int intersect( const Ray& ray , Vec& ip) const
	{
		const Vec& v(ray.org - org);
		const Vec& d(ray.dir);

		double vd = -2*(v*d);
		double dd = 2*(d*d);
		double vv = v*v;
		double rr = square(radius);
		double det = square(vd) - 2*dd*(vv-rr);
		if (det < 0) return 0;
		double sdet = sqrt(det);
		double t1 = vd + sdet; t1/=dd;
		double t2 = vd - sdet; t2/=dd;
		
		if (t1<0 && t2<0) return 0;
		double t=0;
		if (t1<0) t = t2; //t1 is behind
		else if (t2<0) t = t1; //t2 is behind
		else t = min(t1,t2); //both are in front.. find the closest
	
		ip = ray.org + ray.dir*t;
		return 1;
	}
};

class Triangle: public Prim
{
  public:
	Vec V1;
	Vec V2;
	Vec normal;

	Triangle()
	{
		
	}

	Triangle(const Triangle& t)
	{
		V1 = t.V1;
		V2 = t.V2;
		normal = t.normal;
		org = t.org;
		id = t.id;
		material = t.material;
	}

	Vec calcNormal( )
	{
		normal = crossProduct( (V1-org), (V2-org) ).normal();
	}

	Vec getNormal(const Vec& ip) const
	{
		return normal;
	}
	//ip is intersection point
	int intersect2( const Ray& ray , Vec& ip) const
	{
		Vec x1(V1 - org);
		Vec x2(V2 - org);
		Vec d(-ray.dir);
		
		if (!invert(x1,x2,d)) return 0; // A^-1

		Vec t = linearTransform( x1,x2,d, (ray.org-org)); // A^-1x = parametrics
		if (t[2] < 0 || t[0] + t[1] > 1 || t[0] < 0 || t[1] < 0) return 0;

		ip = ray.org + ray.dir*t[2];
		return 1;
	}

/*
This intersection code seems to have better numerical stability than my own.
It also elegantly handles early exists. However, it is much much slower
than my own code. Possibly because of the overlap and lack of normal caching

@article{MollerTrumbore97,
  author = "Tomas Möller and Ben Trumbore",
  title = "Fast, Minimum Storage Ray-Triangle Intersection",
  journal = "journal of graphics tools",
  volume = "2",
  number = "1",
  pages = "21-28",
  year = "1997",
}
*/
	int intersect3(const Ray& ray, Vec& ip) const
	{
		/* find vectors for two edges sharing vert0 */
		const Vec vert0 = org;
		const Vec vert1 = V1;
		const Vec vert2 = V2;
		Vec edge1 = vert1 - vert0;
		Vec edge2 = vert2 - vert0;

		/* begin calculating determinant - also used to calculate U parameter */
		const Vec dir = ray.dir;
		Vec pvec = crossProduct(dir,edge2);

		/* if determinant is near zero, ray lies in plane of triangle */
		double det = edge1 * pvec;
//		const double EPSILON = 1e-24;
		const double EPSILON = 0.000001;

		if (det < EPSILON)
			return 0;

		/* calculate distance from vert0 to ray origin */
		Vec tvec = ray.org - vert0;
		
		/* calculate U parameter and test bounds */
		double u = tvec * pvec;
		if (u < 0.0 || u > det)
		   return 0;
		
		/* prepare to test V parameter */
		Vec qvec = tvec * edge1;
		
		 /* calculate V parameter and test bounds */
		double v = dir * qvec;
		if (v < 0.0 || u + v > det)
		   return 0;
		
		/* calculate t, scale parameters, ray intersects triangle */
		double t = edge2 * qvec;
		double inv_det = 1.0 / det;
		t *= inv_det;
		u *= inv_det;
		v *= inv_det;
	
		if (t < 0) return 0;

		ip = ray.org + (ray.dir*t);
		return 1;
	}

	int intersect(const Ray& ray , Vec& ip) const
	{

  		/* find vectors for two edges sharing vert0 */
		const Vec vert0 = org;
		const Vec vert1 = V1;
		const Vec vert2 = V2;
		Vec edge1 = vert1 - vert0;
		Vec edge2 = vert2 - vert0;

		/* begin calculating determinant - also used to calculate U parameter */
		const Vec dir = ray.dir;
		Vec pvec = crossProduct(dir,edge2);

		/* if determinant is near zero, ray lies in plane of triangle */
		double det = edge1 * pvec;
		const double EPSILON = 0.000001;
		if (det > -EPSILON && det < EPSILON)
			return 0;
		double inv_det = 1.0 / det;

		/* calculate distance from vert0 to ray origin */
		Vec tvec = ray.org - vert0;

		/* calculate U parameter and test bounds */
		double u = tvec * pvec * inv_det;
		if (u < 0.0 || u > 1.0)
			return 0;

		/* prepare to test V parameter */
		Vec qvec = crossProduct(tvec,edge1);

		/* calculate V parameter and test bounds */
		double v = dir * qvec * inv_det;
		if (v < 0.0 || u + v > 1.0)
			return 0;

		/* calculate t, ray intersects triangle */
		double t = edge2 * qvec * inv_det;

		ip = ray.org + (ray.dir*t);
		return 1;
	}
};


typedef std::vector<Prim*> PL;

inline LV addLights()
{
	LV l;

	l.push_back( Light(Vec(-5,-5,-10),ColourF(0.7,1.0,1.0)) );
	l.push_back( Light(Vec(-8,-3,-9),ColourF(1.0,.2,.2)) );
//	l.push_back( Light(Vec(0,0,-2),ColourF(0.7,1.0,1.0)) );
//	l.push_back( Light(Vec(0,0,-2),ColourF(1.0,.2,.2)) );


	return l;
}



//this is too damn slow.
void genPrimaryRays(const Vec& camera, const Screen& screen, RL& rays)
{
	size_t j = 0;
	for (ArrayIndex i(0); screen.isValidIndex(i); screen.incrIndex(i))
	{
		rays[j].org = camera;
		rays[j].dir = (screen.getPoint(i) - camera).normal();
		j++;
	}
}


//todo make proper scene!
PL buildScene(const string& pathMask)
{
	cout<<"Reading input volume... "<<flush;
	inputVol = rysci::IO::VTK::read< unsigned char >(pathMask);
	cout<<"done!\n";
	PL primList;

	//read from a file or hardcode it in or whatever!
	Sphere sphere;
	sphere.id = 0;
	sphere.radius = inputVol->attribs().spacing[0];
	sphere.org = Vec(0,0,0);
	sphere.material = Material(.9,10,1,1,ColourF(.3,.7,.9));

	cout<<"Creating spheres... "<<flush;
	typedef unsigned char Px;
	for (ArrayIndex i(0,0,0); inputVol->isNotPastEnd(i); inputVol->incrIndex(i))
	{
		if ( (*inputVol)[i] )
		{
			ArrayIndex ixm(i); ixm[0]--;
			ArrayIndex ixp(i); ixp[0]++;
			ArrayIndex iym(i); iym[1]--;
			ArrayIndex iyp(i); iyp[1]++;
			ArrayIndex izm(i); izm[2]--;
			ArrayIndex izp(i); izp[2]++;
			if (!( (*inputVol)[ixm] && (*inputVol)[ixp] &&
				(*inputVol)[iym] && (*inputVol)[iyp] &&
				(*inputVol)[izm] && (*inputVol)[izp] ))

			{
				
 				sphere.id++;
				sphere.org = inputVol->getPoint(i);
				primList.push_back(new Sphere(sphere));
			}
		}
	}
	cout<<"done!\nCounted "<<sphere.id<<" spheres\n";
	return primList;
}

//todo template this, remove dot operator, make a dot function
inline ColourF vmult(const ColourF& a, const ColourF& b)
{
	return  ColourF(a[0]*b[0],a[1]*b[1],a[2]*b[2]);

}

inline ColourF clamp(const ColourF& a)
{
	return ColourF( min(a[0],1.0), min(a[1],1.0), min(a[2], 1.0));
}


bool inShadow(const PL& prims, const Prim& p, const Ray& ray)
{
	Vec ip2(0);
	for(PL::const_iterator primp = prims.begin(); primp!= prims.end(); ++primp)
	{
		const Prim& prim = *(*primp);
		if (prim.id == p.id)
			continue;
		if (prim.intersect(ray,ip2))
			return true;
	}
	return false;
}

inline Colour ccast(const ColourF& a)
{
	ColourF b(clamp(a)); //ooh HDR!!!
	return Colour(	static_cast<unsigned char>(b[0]*255),
			static_cast<unsigned char>(b[1]*255),
			static_cast<unsigned char>(b[2]*255) );
}


inline int getCol(const Colour& c)
{
	return makeacol32(c[0],c[1],c[2],255);
}

public BoxList
{
	vector<Box> boxes;

  public:
	void addPrim(const Prim* prim)
	{
		//right now searches bounding box
		//for triangles find if any part of the triangle is inside
		//sphere just does center right now.
		if (std::count(boxes.begin(),boxes.end(),_1->boundBox(prim->org)) == 0)
		{
			//add a new box
		} else {
			//for each box
				//if inside.. add it!
		}

	}

	void removePrim(const Prim* prim)
	{
		for_each(boxes.begin(),boxes.end(),bind1st(Box::remove(),prim));
	}

	
}

public Box
{
	PL primList;

  public:
	Vec bounds[2]; //min is close corner, max is far corner

	bool empty()
	{
		return primList.empty();
	}

	void remove(const Prim* prim)
	{
		primList.remove(prim);
	}

	bool boundBox(const Vec& p)
	{
		if (p[0] > bounds[1][0] || p[0] < bounds[0][0]) return false;
		if (p[1] > bounds[1][1] || p[1] < bounds[0][1]) return false;
		if (p[2] > bounds[1][2] || p[2] < bounds[0][2]) return false;
		return true;
	}

	// Smits’ method
	bool intersect(const Ray &r) const {
		double t0 = 0;
		double t1 = 1000;
		double tmin, tmax, tymin, tymax, tzmin, tzmax;
		if (r.dir[0] >= 0) {
			tmin = (bounds[0][0] - r.org[0]) / r.dir[0];
			tmax = (bounds[1][0] - r.org[0]) / r.dir[0];
		} else {
			tmin = (bounds[1][0] - r.org[0]) / r.dir[0];
			tmax = (bounds[0][0] - r.org[0]) / r.dir[0];
		}
		if (r.dir[1] >= 0) {
			tymin = (bounds[0][1] - r.org[1]) / r.dir[1];
			tymax = (bounds[1][1] - r.org[1]) / r.dir[1];
		} else {
			tymin = (bounds[1][1] - r.org[1]) / r.dir[1];
			tymax = (bounds[0][1] - r.org[1]) / r.dir[1];
		if ( (tmin > tymax) || (tymin > tmax) )
			return false;
		if (tymin > tmin)
			tmin = tymin;
		if (tymax < tmax)
			tmax = tymax;
		if (r.dir[2] >= 0) {
			tzmin = (bounds[0][2] - r.org[2]) / r.dir[2];
			tzmax = (bounds[1][2] - r.org[2]) / r.dir[2];
		} else {
			tzmin = (bounds[1][2] - r.org[2]) / r.dir[2];
			tzmax = (bounds[0][2] - r.org[2]) / r.dir[2];
		}
		if ( (tmin > tzmax) || (tzmin > tmax) )
			return false;
		if (tzmin > tmin)
			tmin = tzmin;
		if (tzmax < tmax)
			tmax = tzmax;
		return ( (tmin < t1) && (tmax > t0) );
	}
};


Colour getPixelColour(const PL& prims, const LV& lights, Ray& ray)
{
	ColourF px(black);
	Vec ip(0);
	double minDist = 3.4e38;

	for(PL::const_iterator primp = prims.begin(); primp!= prims.end(); ++primp)
	{	
		const Prim& prim = *(*primp);
		
		if ( prim.intersect( ray, ip ) )
		{
			Vec sub = ip-ray.org;
			double distance2 = sub*sub;
			if ( minDist < distance2 ) continue;
			minDist = distance2;
			px = black; //new minimum
	
			for (LV::const_iterator light = lights.begin(); light != lights.end(); ++light)
			{
				Vec ldir = (light->org - ip).normal();
				double dot = ldir * prim.getNormal(ip);
				if (dot >= 0) //check if on outside of surface
				{	
				//	if (!inShadow(prims, prim, Ray(ip,ldir)))
					{
						double diff = dot * prim.material.diffuse;
						diff = pow(diff,prim.material.specular) + diff;
						px += vmult(prim.material.colour, light->colour) * diff;
					}
				}
			}
		}
	}
	return ccast(px);
}

int main(int argc, char** argv)
{
	if (argc != 2)
	{
		cout<<"Usage: "<<argv[0]<<" filePath"<<endl;
	}

	VolAttribs scr;
	const size_t screen_w = 700;
	const size_t screen_h = 700;

	scr.size[0] = screen_w; scr.size[1] = screen_h; scr.size[2] = 1;
	scr.spacing[0] = 5.0/static_cast<double>(screen_w);
	scr.spacing[1] = 5.0/static_cast<double>(screen_w);
	scr.spacing[2] = 0;
	scr.origin[0] = -scr.size[0]/2*scr.spacing[0];
	scr.origin[1] = -scr.size[1]/2*scr.spacing[1];
	scr.origin[2] = -2;
	Vec camera(0,0,-7);	

	//only allocate once in program (or every screen size change!)	
	cout<<"Generating Primary Rays... "<<flush;
	Screen screenc(scr);
	RL rays(screenc.size());
	//only compute when camera or screen changes
	genPrimaryRays( camera, screenc, rays);
	cout<<"done!\n";

	cout<<"Loading volume... "<<flush;	
	PL prims = buildScene(string(argv[1]));
	cout<<"done!\n";

	cout<<"Adding Lights.. "<<flush;
	LV lights = addLights();
	cout<<"done!\n"<<endl;

	allegro_init();
	install_keyboard();
	set_color_depth(32);
	set_gfx_mode(GFX_AUTODETECT_WINDOWED, screen_w, screen_h, 0, 0);
	BITMAP* buffer = create_system_bitmap(scr.size[0],scr.size[1]);



	//this is the ray tracer right here! all self-contained!
	//re-write transform to make it a freely parallel map
	int frame(0);
	const double theta = 1.2;
	double sliceNum(10000); //to appease mencoder which things 11 comes after 1.
	double speed = 0.005;
	while(true)
	{
		cout<<"Frame number: "<<frame++<<" computing..."<<flush;
		double t = 0; double theta = 0;
		if (key[KEY_ESC])
			break;
		if (key[KEY_UP])
			t = 0.05;
		if (key[KEY_DOWN])
			t = 0.05;
		if (key[KEY_LEFT])
			theta = 0.05;	
		if (key[KEY_RIGHT])
			theta = 0.05;
/*		
		for(PL::const_iterator primp = prims.begin(); primp!= prims.end(); ++primp)
		{
			const Prim& prim = *(*primp);		
			prim.org[1] *= cos(t) * sin(theta);
			prim.org[2] *= sin(t) * sin(theta);
			prim.org[0] *= cos(t);
		}
*/
		sliceNum++;
		size_t i = 0;
		for (int y = 0; y < screen_w ; y++)
		{
			for (int x=0;x<screen_h;x++)
			{
				_putpixel32(buffer, x, y, getCol(getPixelColour(prims,lights, rays[i++])));
			}
		}
		cout<<"done!\n";
		blit(buffer, screen, 0, 0, 0, 0, screen_w, screen_h);
	}

	//c++ can be ugly!
	for(PL::const_iterator primp = prims.begin(); primp!= prims.end(); ++primp){ delete *primp; }
	prims.clear();

	destroy_bitmap(buffer);
	cout<<"Hello World\n"<<sliceNum<<endl;
	return 0;
}END_OF_MAIN();
