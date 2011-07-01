/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxcontroller;

import java.util.concurrent.*;

/**
 *
 * @author mguerra
 */
public class Server extends Thread
{
   private BlockingQueue<Request> queue = new LinkedBlockingQueue<Request>();

   public void accept(Request request)
   {
      queue.add(request);
   }

   @Override
   public void run()
   {
      while (true)
      {
         try
         {
            execute(queue.take());
         }
         catch (InterruptedException e) 
         {

         }
      }
   }

   private void execute(final Request request)
   {
      new Thread( new Runnable()
      {
         @Override
         public void run()
         {
            request.execute();
         }
      } ).start();
   }
}
