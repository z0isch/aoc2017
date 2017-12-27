using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ConsoleApp1
{
    public static class Day23
    {
        public static void Part2()
        {
            int a = 1, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0;
            b = 57;
            c = b;
            b = b * 100;
            b = b + 100000;
            c = b;
            c = c + 17000;
            while(b != c)
            {
                f = 1;
                d = 2;
                while(d != b)
                {
                    e = 2;
                    while(e != b)
                    {
                        if (d * e == b) f = 0;
                        e = e + 1;
                    }
                    d = d + 1;
                }
                if(f==0) h++;
                b = b + 17;
            }
        }
    }
}
