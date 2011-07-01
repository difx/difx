/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxutilities;

/**
 *
 * @author mguerra
 */
public class Elapsed {

   public static String calcHMS(int timeInSeconds)
   {
      int hours, minutes, seconds;

      hours         = timeInSeconds / 3600;
      timeInSeconds = timeInSeconds - (hours * 3600);
      minutes       = timeInSeconds / 60;
      timeInSeconds = timeInSeconds - (minutes * 60);
      seconds       = timeInSeconds;

      return( String.format("%02d", hours)   + ":" +
              String.format("%02d", minutes) + ":" +
              String.format("%02d", seconds) );
   }

   public static void main(String[] args)
   {
      Elapsed elap = new Elapsed();
      String hhmmss = Elapsed.calcHMS(10000);
      System.out.println(hhmmss);
   }
}