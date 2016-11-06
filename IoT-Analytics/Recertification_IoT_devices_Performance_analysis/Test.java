
import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Random;



public class Test {



	
static Random random = new Random();
    
    public static void main(String[] args) {
        
        
        
        try{
        
          PrintStream my_output = new PrintStream(new File("output.txt"));
          System.setOut(my_output);

        } 
        catch(FileNotFoundException f){
                          System.out.println(f);
        
        }
        
        double lambda1 = 1.0/17.98;
        double lambda2 = 1.0/10;
        
        int a = 0;
        
       while(a<30){
    	   
    	   double begin_clock = 0;
           double begin_arrival = 2;

           double inter_arrival_time = hasNext_exp(lambda1);
           double one_service_time = Double.parseDouble(args[0]);//
           double time_between_retransmission = hasNext_exp(lambda2);
           int max_queue_size = Integer.parseInt(args[1]);//2;
           double master_clock = begin_clock;
           double CLA = begin_arrival;
           Double CLS = null;
           double next;
           Double[] mean_T = new Double[30];
           Arrays.fill(mean_T, 0d);
           Double[] mean_D = new Double[30];
           Arrays.fill(mean_D, 0d);
           Double[] P = new Double[30];
           Arrays.fill(P, 0d);
           Double[] percentile_T = new Double[30];
           Arrays.fill(percentile_T, 0d);
           Double[] percentile_D = new Double[30];
           Arrays.fill(percentile_D, 0d);
           
           
           
           ArrayList<Device> iotdevices = new ArrayList<Device>();
           
           Queue<Device> service_queue = new LinkedList<Device>();
           PriorityQueue<Device> CLR = new PriorityQueue<Device>(max_queue_size,new Comparator<Device>(){
           	@Override
           	public int compare(Device d1, Device d2) {
       			return Double.compare(d1.reorbiting_time,d2.reorbiting_time);
       		}
       	});
           
           
       
    	   
       while(iotdevices.size() < 1000) {    

            if (CLR.isEmpty()) {
                if (CLS == null) {

                    master_clock = CLA;
                    Device temp=new Device(CLA);
                    temp.reorbiting_time=master_clock;
                    CLA = CLA + inter_arrival_time;
                    CLA=((double)(Math.round(CLA*100.0))/100.0);
                    CLS = master_clock + one_service_time;
                    CLS=((double)(Math.round(CLS*100.0))/100.0);
                    
                    temp.departure_time=CLS;
                    service_queue.add(temp);
                    
                    iotdevices.add(temp);
                    
                    continue;

                } else if (CLA <= CLS) {
                    master_clock = CLA;
                    Device temp=new Device(CLA);

                    CLA = CLA + inter_arrival_time;
                    CLA=((double)(Math.round(CLA*100.0))/100.0);

                    if (service_queue.size() < max_queue_size) {

                        
                        temp.reorbiting_time=master_clock;
                        service_queue.add(temp);

                    } else {
                    	next = master_clock + time_between_retransmission;
                    	next=((double)(Math.round(next*100.0))/100.0);

                    	temp.reorbiting_time=next;

                    	CLR.add(temp);
                    }
                    continue;
                    
                } else {

                    master_clock = CLS;
                    Device temp = service_queue.poll();
                    
                    temp.departure_time = master_clock;
                    iotdevices.add(temp);
                    
                    if(!service_queue.isEmpty()){
							CLS = master_clock + one_service_time;
							CLS = ((double) (Math.round(CLS * 100.0)) / 100.0);
                    }
                    else{
                     CLS = null;	
                    }
                    continue;

                }

            } else {

                double clr = CLR.element().reorbiting_time;
               
                
                if (clr <= CLA && (clr <= CLS||CLS==null)) {

                    Device temp = CLR.poll();
                    master_clock = temp.reorbiting_time;

                    if (service_queue.size() < max_queue_size) {
                    	service_queue.add(temp);
                    	
                        continue; 
                       
                    } else {

                    	next = master_clock + time_between_retransmission;
                    	next=((double)(Math.round(next*100.0))/100.0);
                    	temp.reorbiting_time=next;
                    	CLR.add(temp);
                        continue;
                    }
                }
                if (CLA < clr && (CLA <= CLS||CLS==null)) {
                    
                    master_clock = CLA;
                    Device temp=new Device(CLA);
                    CLA = CLA + inter_arrival_time;
                    CLA=((double)(Math.round(CLA*100.0))/100.0);
                    
                    if (service_queue.size() < max_queue_size) {
                    	
                        temp.reorbiting_time=master_clock;
                        service_queue.add(temp);
                      
                        continue;

                    } else {
                    	
                    	next = master_clock + time_between_retransmission;
                    	next=((double)(Math.round(next*100.0))/100.0);

                    	temp.reorbiting_time=next;
                     	CLR.add(temp);
                        continue;
                    }

                }

                if (CLS < clr && CLS < CLA) {

                    master_clock = CLS;

                    Device temp = service_queue.poll();
                    temp.departure_time=master_clock;
                    iotdevices.add(temp);
 
                     
                    if(!service_queue.isEmpty()){
							CLS = master_clock + one_service_time;
							CLS = ((double) (Math.round(CLS * 100.0)) / 100.0);
                    }
                    else{
                     CLS = null;	
                    }
                    continue;
                }

            }

        }
       
     
       //System.out.println(a);
       
       for(int y = 0; y < iotdevices.size(); y++){
    	   mean_D[a] += Math.round((iotdevices.get(y).reorbiting_time-iotdevices.get(y).arrival_time));
           mean_T[a] += Math.round((iotdevices.get(y).departure_time-iotdevices.get(y).arrival_time));
       }
       
       mean_D[a] = (mean_D[a]/iotdevices.size());
       mean_T[a] = (mean_T[a]/iotdevices.size());
       
       System.out.println(mean_T[a]);
       
       
    
       P[a] = (double)Math.round(iotdevices.get(999).departure_time-iotdevices.get(0).arrival_time);
       
       
       
       Collections.sort(iotdevices, new Comparator<Device>() {
    	   
    	@Override
       	public int compare(Device d1, Device d2) {
   			return Double.compare(d1.departure_time-d1.arrival_time, d2.departure_time-d2.arrival_time);
   		}
   	});
       
        percentile_T[a]=iotdevices.get(949).departure_time-iotdevices.get(949).arrival_time;    
        
        System.out.println((iotdevices.get(949).departure_time-iotdevices.get(949).arrival_time));
        
        System.out.println(mean_D[a]);
        
        Collections.sort(iotdevices, new Comparator<Device>() {
     	   
        	@Override
           	public int compare(Device d1, Device d2) {
       			return Double.compare(d1.reorbiting_time-d1.arrival_time, d2.reorbiting_time-d2.arrival_time);
       		}
   	});
        percentile_D[a]=iotdevices.get(949).reorbiting_time-iotdevices.get(949).arrival_time;
        System.out.println((iotdevices.get(949).reorbiting_time-iotdevices.get(949).arrival_time));
        
        System.out.println(P[a]);
        
        a++;
       
       }  
        
        
    }
      
    

    public static double hasNext_exp(double lambda){
    
        double x = Math.log(1-random.nextDouble())/(-lambda);
  
       return ((double)(Math.round(x*100.0))/100.0);
        
    }
	
	
}
