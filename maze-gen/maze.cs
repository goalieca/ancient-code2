using System;
using System.Collections.Generic;

public class Maze
{
    const int width = 1000;
    const int height = 1000;
    const int forwardness = 4;

    const char visited = ' ';
    const char unvisited = '@';
    char[,] maze = new char[height,width];

    enum Direction {North=0,South,East,West};
    Random randomGenerator = new Random(42);

    /** 
     * Retarded c# has no cleaner way of doing it
     */
    void emptyMaze()
    {
        for (int y=0; y<height; y++)
        {
            for (int x=0; x<width; x++)
            {
                maze[y,x] = unvisited;
            }
        }
    }

//todo: make a getter :D
    struct Point
    {
        int x;
        int y;
    
        public int X
        {
            get {
                return x;
            }
            set {
                x = value;
            }
        }

        public int Y
        {
            get {
                return y;
            }
            set {
                y = value;
            }
        }

        public Point(int x,int y)
        {
            this.x = x;
            this.y = y;
        }

        public override String ToString()
        {
            return "(" + x + ", " + y + ")";
        }
    }

    Point movePoint(Point p, Direction d)
    {
        switch (d)
        {
            case Direction.North:
                p.Y--;
                break;
            case Direction.South:
                p.Y++;
                break;
            case Direction.East:
                p.X++;
                break;
            case Direction.West:
                p.X--;
                break;
        }
        return p;
    }

    bool notBorder(Point p, Direction d)
    {
        switch (d)
        {
            case Direction.North:
                return p.Y > 0;
            case Direction.South:
                return p.Y < height - 1;
            case Direction.East:
                return p.X < width - 1;
            case Direction.West:
                return p.X > 0;
            default:
                return false;
        }
    }

    bool cornered(Point p)
    {
        if (isValidDir(p,Direction.North))
            return false;
        if (isValidDir(p,Direction.South))
            return false;
        if (isValidDir(p,Direction.East))
            return false;
        if (isValidDir(p,Direction.West))
            return false;
        return true;
    }

    bool notBreakingWall(Point p, Direction d)
    {
        Point pn = movePoint(p,Direction.North);
        Point pe = movePoint(p,Direction.East);
        Point ps = movePoint(p,Direction.South);
        Point pw = movePoint(p,Direction.West);
        
        switch (d)
        {
            case Direction.North:
                return !(maze[pn.Y,pn.X] == visited || maze[pw.Y,pw.X] == visited || maze[pe.Y,pe.X] == visited );
            case Direction.South:
                return !(maze[ps.Y,ps.X] == visited || maze[pw.Y,pw.X] == visited || maze[pe.Y,pe.X] == visited );
            case Direction.East:
                return !(maze[pn.Y,pn.X] == visited || maze[ps.Y,ps.X] == visited || maze[pe.Y,pe.X] == visited );
            case Direction.West:
                return !(maze[pn.Y,pn.X] == visited || maze[pw.Y,pw.X] == visited || maze[ps.Y,ps.X] == visited );
            default:
                return false;
        }
    }

    bool isValidDir(Point p, Direction d)
    {
        Point np = movePoint(p,d);
        return notBorder(np,d) && (maze[np.Y,np.X] == unvisited) && notBreakingWall(np,d);
    }

    void printMaze()
    {
        for (int y=0; y<height; y++)
        {
            for (int x=0; x < width; x++)
            {
                Console.Write(maze[y,x]);
            }
            Console.Write('\n');
        }
    }
    
    //for debuggin. puts * in current position
    void printMaze(Point cur)
    {
        for (int y=0; y<height; y++)
        {
            for (int x=0; x < width; x++)
            {
                if ( x == cur.X && y == cur.Y )
                    Console.Write('*');
                else
                    Console.Write(maze[y,x]);
            }
            Console.Write('\n');
        }
    }

    Direction getRandomDir()
    {
        switch( randomGenerator.Next(4) )
        {
            case 0:
                return Direction.North;
            case 1:
                return Direction.East;
            case 2:
                return Direction.West;
            case 3:
                return Direction.South;
            default: //to appease c#
                return Direction.North;
        }
    }

    Point makeSeed()
    {
        int x = randomGenerator.Next(1,width-1);
        int y = randomGenerator.Next(1,height-1);
        return new Point(x,y);
    }

    void genMaze(Point seed)
    {
        Point pos = seed;
        
        //todo: generics!
        //good initial guess for stack size
        Stack<Point> path = new Stack<Point>(height*2+width*2);
        maze[pos.Y,pos.X] = visited;
        path.Push(pos);

        Direction dir = getRandomDir();
        int fwd_cnt = forwardness;

        while (path.Count != 0)
        {
            if (isValidDir(pos,dir))
            {
                path.Push(pos);
                pos = movePoint(pos,dir);
                maze[pos.Y,pos.X] = visited;

                if (--fwd_cnt == 0)
                {
                    fwd_cnt = forwardness;
                    dir = getRandomDir();
                }
                continue;
            } else if (cornered(pos))
            {
                pos = (Point) path.Pop();
                fwd_cnt = forwardness;
                dir = getRandomDir();
                continue;
            } else { //not cornered but can't go in current direction
                fwd_cnt = forwardness;
                dir = getRandomDir();
                continue;
            }
        }
    }

    public static int Main(string[] args)
    {
        Maze maze = new Maze();
        maze.emptyMaze();
    
        Point seed = maze.makeSeed();

        maze.genMaze(seed);
        maze.printMaze();
        return 0;
    }
}
