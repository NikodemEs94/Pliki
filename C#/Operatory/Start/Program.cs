using System;

namespace Start
{
    class Program
    {
        static void Main(string[] args)
        {
            //int a = 11;
            //int b = 3;
            int c = 4;

            /*
            Console.WriteLine($"{a}+{b}={a + b}");

            Console.WriteLine($"{a}%{b}={a % b}" + $" Reszta z dzielenia wynosi {a % b}");

            c++;
            Console.WriteLine(c);

            c--;
            Console.WriteLine(c);

            Console.WriteLine(Math.Pow(4, 3));
            */

            c = 6;
            c += 3;
            c -= 5;
            c *= 7;
            c /= 3;
            c %= 4;

            Console.WriteLine(c);


            Console.ReadKey();
        }
    }
}
