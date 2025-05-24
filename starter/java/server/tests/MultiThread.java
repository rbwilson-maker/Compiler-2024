import java.util.concurrent.atomic.AtomicInteger;

/**
 *
  * @author zkieda
   */
   public class MultiThread {
       static AtomicInteger count = new AtomicInteger();
           /**
                * IMPORTANT : new thread IO is not being redirected to client as well.
                     */
                         static class CT extends Thread {
                                 final int val;
                                         public CT(int val){
                                                     this.val = val;
                                                                 count.addAndGet(val);
                                                                         }
                                                                                 @Override
                                                                                         public void run() {
                                                                                                     try {
                                                                                                                     System.out.println("aa");
                                                                                                                                     Thread.sleep(1000);
                                                                                                                                                     System.out.println("aa");
                                                                                                                                                                 } catch (Exception e) {
                                                                                                                                                                                 e.printStackTrace();
                                                                                                                                                                                             }
                                                                                                                                                                                                         if(val != 0) {
                                                                                                                                                                                                                         Thread t = new CT(val-1);
                                                                                                                                                                                                                                         t.start();
                                                                                                                                                                                                                                                         try {
                                                                                                                                                                                                                                                                                             t.join();

                                                                                                                                                                                                                                                                                                             } catch (Exception e) {
                                                                                                                                                                                                                                                                                                                             }
                                                                                                                                                                                                                                                                                                                                         }
                                                                                                                                                                                                                                                                                                                                                 }
                                                                                                                                                                                                                                                                                                                                                     }

                                                                                                                                                                                                                                                                                                                                                         public static void main(String[] args) throws 
                                                                                                                                                                                                                                                                                                                                                                 Exception{
                                                                                                                                                                                                                                                                                                                                                                         System.out.println("th1");
                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                         Thread t = new CT(10);
                                                                                                                                                                                                                                                                                                                                                                                                 t.start();
                                                                                                                                                                                                                                                                                                                                                                                                         t.join();
                                                                                                                                                                                                                                                                                                                                                                                                                 System.out.println(count.get());
                                                                                                                                                                                                                                                                                                                                                                                                                     }
                                                                                                                                                                                                                                                                                                                                                                                                                     }

