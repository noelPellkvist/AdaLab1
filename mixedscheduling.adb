pragma Priority_Specific_Dispatching(FIFO_Within_Priorities, 2, 30);
pragma Priority_Specific_Dispatching(Round_Robin_Within_Priorities, 1, 1);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

procedure mixedscheduling is
   package Duration_IO is new Ada.Text_IO.Fixed_IO(Duration);
   package Int_IO is new Ada.Text_IO.Integer_IO(Integer);

	Start : Time; -- Start Time of the System
   Calibrator: constant Integer := 1200; -- Calibration for correct timing
   Warm_Up_Time: constant Integer := 100; -- Warmup time in milliseconds

function To_Float(TS : Time_Span) return Float is
     SC : Seconds_Count;
     Frac : Time_Span;
   begin
     Split(Time_Of(0, TS), SC, Frac);
     return Float(SC) + Time_Unit * Float(Frac/Time_Span_Unit);
   end To_Float;

   -- Function F is a dummy function that is used to model a running user program.
   function F(N : Integer) return Integer is
     X : Integer := 0;
   begin
     for Index in 1..N loop
       for I in 1..500 loop
         X := X + I;
       end loop;
     end loop;
     return X;
   end F;

task Watchdog_Timer is
	pragma Priority(10);
	entry CheckHealth;
   end Watchdog_Timer;

   task body Watchdog_Timer is

	LastTime : Time;
   begin
	LastTime := Clock;
     loop
		select 
			accept CheckHealth do		-- this only gets called when the helper task is running	
				if (Clock - LastTime) > Milliseconds(1220) then
					Put_Line("CRASH");
				end if;
				LastTime := Clock;
				delay until Ada.Real_Time.Clock + Milliseconds(1100);
			end CheckHealth;
			or
				delay until Ada.Real_Time.Clock + Milliseconds(100);
		end select;
     end loop;
   end Watchdog_Timer;

task Helper is
	pragma Priority(2);
   end Helper;

   task body Helper is
	LastTime : Time;
   begin
	LastTime := Clock;
     loop
		Watchdog_Timer.CheckHealth; --Checking the health every time the helper task runs
		delay until Ada.Real_Time.Clock + Milliseconds(100);
     end loop;
   end Helper;

task type T(Id: Integer; Prio: Integer; Phase: Integer; Period : Integer; Computation_Time : Integer; Relative_Deadline: Integer) is
     pragma Priority(Prio); -- A higher number gives a higher priority
   end;

   task body T is
     Next : Time;
     Release: Time;
     Completed : Time;
     Response : Time_Span;
     Average_Response : Float;
     Absolute_Deadline: Time;
     WCRT: Time_Span;
     Dummy : Integer;
     Iterations : Integer;
     Healthy_System : Boolean := True;
begin
     Release := Clock + Milliseconds(Phase);
     delay until Release;
     Next := Release;
     Iterations := 0;
     WCRT := Milliseconds(0);

     loop
       Next := Release + Milliseconds(Period);
       Absolute_Deadline := Release + Milliseconds(Relative_Deadline);

       for I in 1..Computation_Time loop
         Dummy := F(Calibrator);
       end loop;

       Completed := Clock;
       Response := Completed - Release;

       if Completed > Absolute_Deadline then -- this is the flag that signals a fault
         Healthy_System := False;
       end if;

       Average_Response := (Float(Iterations) * Average_Response + To_Float(Response)) / Float(Iterations + 1);
       if Response > WCRT then
         WCRT := Response;
       end if;
       Iterations := Iterations + 1;

       Put("FIFO Task ");
       Int_IO.Put(Id, 1);
       Put("- Release: ");
       Duration_IO.Put(To_Duration(Release - Start), 2, 3);
       Put(", Completion: ");
       Duration_IO.Put(To_Duration(Completed - Start), 2, 3);
       Put(", Response: ");
       Duration_IO.Put(To_Duration(Response), 1, 3);
       Put(", WCRT: ");
       Ada.Float_Text_IO.Put(To_Float(WCRT), fore => 1, aft => 3, exp => 0);
       Put(", Next Release: ");
       Duration_IO.Put(To_Duration(Next - Start), 2, 3);
       Put_Line("");

       Release := Next;
       delay until Release;
	   
     end loop;
   end T;
   
   		
task type Round_Robin(Id: Integer; Phase: Integer; Period : Integer; Computation_Time : Integer; Relative_Deadline: Integer) is
   pragma Priority(1);
end Round_Robin;
 						
task body Round_Robin is
   Next : Time;
     Release: Time;
     Completed : Time;
     Response : Time_Span;
     Average_Response : Float;
     Absolute_Deadline: Time;
     WCRT: Time_Span;
     Dummy : Integer;
     Iterations : Integer;
     Healthy_System : Boolean := True;
begin
     Release := Clock + Milliseconds(Phase);
     delay until Release;
     Next := Release;
     Iterations := 0;
     WCRT := Milliseconds(0);

     loop
       Next := Release + Milliseconds(Period); -- the period should be 0 since the tasks are repeating
       Absolute_Deadline := Release + Milliseconds(Relative_Deadline);

       for I in 1..Computation_Time loop
         Dummy := F(Calibrator);
       end loop;

       Completed := Clock;
       Response := Completed - Release;

       Iterations := Iterations + 1;

       Put("RR   Task ");
       Int_IO.Put(Id, 1);
       Put(" running!");
       Put_Line("");

       Release := Next;
       delay until Release;
	   
     end loop;
   end Round_Robin;

   
   Task_1 : T(1, 5, Warm_Up_Time, 300, 100, 300);
   Task_2 : T(2, 4, Warm_Up_Time, 400, 100, 400);
   Task_3 : T(3, 3, Warm_Up_Time, 600, 100, 600);
   Task_4 : T(4, 2, Warm_Up_Time, 1200, 200, 1200);
   
   RR_1 : Round_Robin(1, Warm_Up_Time, 0, 100, 1200); -- the relative deadline is one hyperperiod
   RR_2 : Round_Robin(2, Warm_Up_Time, 0, 100, 1200);
   RR_3 : Round_Robin(3, Warm_Up_Time, 0, 100, 1200);
   
begin
	Start := Clock; -- Central Start Time
null;
end mixedscheduling;


