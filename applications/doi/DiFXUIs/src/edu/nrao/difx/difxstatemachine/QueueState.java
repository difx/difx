/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxstatemachine;

import edu.nrao.difx.difxdatamodel.*;

/**
 *
 * @author mguerra
 */
public interface QueueState
{   
   public void add(Queue queue, Job job);
   public void addRun(Queue queue, Job job);
   public void addRunNow(Queue queue, Job job);
   public boolean remove(Queue queue, Job job);
   public boolean removeRun(Queue queue, Job job);
   public void run(Queue queue);
   public void pause(Queue queue);
   public void done(Queue queue, Queue.QueueStopEvents qEvt);
   public void stop(Queue queue);
   public void stopRun(Queue queue, Job job);
   public void accept(Queue queue);
   public void changeState(Queue queue, QueueState state);
}

