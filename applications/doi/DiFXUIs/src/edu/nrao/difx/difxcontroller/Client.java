/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxcontroller;

import java.util.Random;

/**
 *
 * @author mguerra
 */
public class Client
{
   public void go()
   {
      final Server server = new Server();
      server.start();

      for (int i = 0; i < 10; i++)
      {
         final Request request = createRequest(i);
         server.accept(request);
      }
   }

   Request createRequest(final int index)
   {
      return new Request()
      {
         @Override
         public void execute()
         {
            for (int i = 0; i <= 100; i += 10)
            {
               sleep((new Random().nextInt(5) + 1) * 1000);
               System.out.println( String.format( "request: %d completed: %d%%", index, i) );
            }

            System.out.println( String.format("reqest %d completed", index));
         }

      };
   }

   private void sleep(int millis) {
      try {
         Thread.sleep(millis);
      }
      catch (InterruptedException e) {

      }
   }

   public static void main(String[] args)
   {
      new Client().go();
   }

}
