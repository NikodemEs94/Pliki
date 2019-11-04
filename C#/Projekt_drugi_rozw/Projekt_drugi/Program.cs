using System;

namespace Projekt_drugi
{
    class Program
    {
        static void Main(string[] args)
        {
            /* Miejsce na komentarz
             * 
             * 
             * 
             * 
             * 
             */




            Console.Beep(500, 700);
            Console.BackgroundColor = ConsoleColor.Blue;
            Console.ForegroundColor = ConsoleColor.Red;

            Console.Title = "Witaj w moim programie";

            Console.SetCursorPosition(8,3);
            Console.WriteLine("Hello World!");
            Console.Write("To jest ");
            Console.WriteLine("WriteLine");

            Console.ResetColor();

            Console.ReadKey();
            Console.Clear();
            Console.Beep(500, 700);
            Console.WriteLine("Wyczyściłeś okno oraz zresetowałeś kolory");
            Console.ReadKey();
        }
    }
}
