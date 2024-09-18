pragma Task_Dispatching_Policy(FIFO_Within_Priorities);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

procedure overloaddetection is
   package Duration_IO is new Ada.Text_IO.Fixed_IO(Duration);
   package Int_IO is new Ada.Text_IO.Integer_IO(Integer);

   Start : Time; -- Start Time of the System
   Calibrator: constant Integer := 800; -- Calibration for correct timing
   Warm_Up_Time: constant Integer := 100; -- Warmup time in milliseconds

   -- Conversion Function: Time_Span to Float
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

   task type Helper is
     entry Check_Health(F : in Boolean);
   end Helper;

   task body Helper is
     Health_Flag : Boolean := True;
   begin
     loop
        accept Check_Health(F : in Boolean) do
			Put_Line("Checking health");
		end Check_Health;
     end loop;
   end Helper;

   task type Watchdog_Timer is
     --entry Health(Flag : Boolean);
	 entry Reset;
   end Watchdog_Timer;

   task body Watchdog_Timer is
     Timer : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
     loop
         accept Reset do
			Timer := Ada.Real_Time.Clock;
           --if Flag then
             -- Ada.Text_IO.Put_Line("System's healthy");
           --else
             -- Ada.Text_IO.Put_Line("Overload.");
           end Reset;
     end loop;
   end Watchdog_Timer;

   -- Workload Model for a Parametric Task
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

       Put("Task ");
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
	   Helper.Check_Health(False);
       delay until Release;
	   
     end loop;
   end T;

   -- Running Tasks
   -- NOTE: All tasks should have a minimum phase, so that they have the same time base!
   Task_1 : T(1, 4, Warm_Up_Time, 300, 100, 300);
   Task_2 : T(2, 3, Warm_Up_Time, 400, 100, 400);
   Task_3 : T(3, 2, Warm_Up_Time, 600, 100, 600);
   Task_4 : T(4, 1, Warm_Up_Time, 1200, 200, 1200);

-- Main Program: Terminates after measuring start time
begin
   Start := Clock; -- Central Start Time
   null;
end overloaddetection;



