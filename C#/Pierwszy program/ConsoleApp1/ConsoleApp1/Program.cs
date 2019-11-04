using System;

namespace ConsoleApp1
{
    class Program
    {
        private static int i;
        private static ConsoleKeyInfo a;

        static void Main(string[] args)
        {
            Console.WriteLine("Hello!");
            Console.WriteLine(2 + 2);
            for (i = 0; i < 5; i++)
            {
                Console.WriteLine(i*3);    
            };
            Console.WriteLine("Hello");
            Console.ReadKey();
            Console.WriteLine();
           
            Console.WriteLine("Ty!");

        }
    }
}
