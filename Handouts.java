/** 
 * Program to estimate average number of handouts received by each ISPOR attendee during
 * a conference.  The conference is assumed to be 3 days, where each day contains a variety
 * of different types of sessions.
 * 
 * For each day there is a probability of attending the conference.
 * For each session type, there is a probability of attending.
 * For each session, there is a probability of having handouts available.
 * For each attendee, there is a probability of taking handouts if available.
 * 
 * All probabilites are beta distributed with parameters a and b.
 * 
 * The simulation is run for 2500 attendees, and the mean and standard deviation of 
 * handouts is outputed.
 * 
 * Algorithm:
 * 1. generate parameters a and b for all probabilities
 * 2. calculate estimated number of handouts for each attendee
 * 3. output summary statistics 
 */


package Main;
import java.io.*;
import java.util.*;

import umontreal.iro.lecuyer.probdist.BetaDist;

public class Handouts {
	
	enum Session {PLENARY, ISSUE, PODIUM, WORKSHOP, FORUM}
	
	static int rndctr=0, maxrnd; //random number counter
	static double[] rand = new double[360000];
	
	public static void main (String[] args) throws IOException{
		
		int numdays = 3, numattend = 2500;
		Session[][] types = {{Session.PLENARY, Session.ISSUE, Session.PODIUM, Session.PODIUM, Session.WORKSHOP, Session.FORUM},
						   {Session.PODIUM, Session.PLENARY, Session.ISSUE, Session.WORKSHOP, Session.WORKSHOP, Session.FORUM},
						   {Session.WORKSHOP, Session.ISSUE, Session.WORKSHOP, Session.WORKSHOP}};
		int dayctr, sesctr, pctr; // day, session, and person counters
		int[] numhandouts = new int[numattend];
		
		PrintWriter outfile = new PrintWriter(new FileWriter("handoutput.dat"));
		double t_begin, t_end;
		
		// read random numbers from file into array
		Scanner rndfile = new Scanner(new FileReader("random.dat")); 
		while(rndfile.hasNext()){
			rand[rndctr++] = rndfile.nextDouble();
		}
		maxrnd = rndctr;
		rndctr = 0; //reset rndctr for future use
		rndfile.close();
			
		t_begin = System.currentTimeMillis(); // start timer
		for (pctr = 0; pctr < numattend; pctr++){ // for every attendee, do something
			numhandouts[pctr] = 0; // initialize number of handouts to zero
			for (dayctr = 0; dayctr < numdays; dayctr++){ 
				if (attendDay(dayctr)){ // if attend that day, then calculate handouts
					for (sesctr = 0; sesctr < types[dayctr].length; sesctr++){ // for every session that day, calculate handouts
						if(attendSession(types[dayctr][sesctr]) && handoutsAvail(types[dayctr][sesctr]) && takeHandout())
							numhandouts[pctr]++;
					}
				}
				
			}
		}
		t_end = System.currentTimeMillis();
		
		// output data
		System.out.println((t_end-t_begin)/1000);
		for (pctr = 0; pctr < numattend; pctr++){
			outfile.println(numhandouts[pctr]); //print to output file
		}
		outfile.close();
	}
	
	static boolean attendDay(int day){
		
		int[] a = {8,8,3};
		int[] b = {2,2,2};
		
		//if random number is less than beta probability then function returns true
		if (rand[rndctr++] < BetaDist.inverseF(a[day], b[day], 10, rand[rndctr++]))
			return true;
		else
			return false;
	}
	
	static boolean attendSession(Session ses){
		
		int a, b;
		
		switch (ses) {
			case PLENARY:
			case ISSUE:
			case FORUM:
				a = 4; b = 6;
				break;
			case WORKSHOP:
			case PODIUM:
				a = 7; b = 3;
				break;
			default:
				a = 1; b = 1;
		}
		
		if (rand[rndctr++] < BetaDist.inverseF(a, b, 10, rand[rndctr++]))
			return true;
		else
			return false;
	}

	static boolean handoutsAvail(Session ses){
		
		int a, b;
		
		switch (ses) {
			case PLENARY:
			case ISSUE:
			case FORUM:
				a = 19; b = 1;
				break;
			case WORKSHOP:
				a = 8; b = 2;
				break;
			case PODIUM:
				a = 6; b = 2;
				break;
			default:
				a = 1; b = 1;
		}
		
		if (rand[rndctr++] < BetaDist.inverseF(a, b, 10, rand[rndctr++]))
			return true;
		else
			return false;
	}
	
	static boolean takeHandout(){	
		
		int a = 6, b = 6;
		
		if (rand[rndctr++] < BetaDist.inverseF(a, b, 10, rand[rndctr++]))
			return true;
		else
			return false;
	}
}
