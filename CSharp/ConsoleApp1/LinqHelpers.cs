using System.Collections.Generic;

namespace ConsoleApp1
{
    static class LinqHelpers
    {
        public static IEnumerable<T> Cycle<T>(this IList<T> xs)
        {
            int i = 0;
            while (true)
            {
                int oldI = i;
                if (i == xs.Count)
                {
                    i = 1;
                    yield return xs[0];
                }
                else
                {
                    i++;
                    yield return xs[oldI];
                }
            }
        }
    }
}