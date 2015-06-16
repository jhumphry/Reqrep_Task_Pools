-- error_handling_example.adb
-- An example of handling errors when using the Reqrep_Task_Pool

-- Copyright (c) 2015, James Humphry
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
-- REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
-- AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
-- INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
-- LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
-- OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
-- PERFORMANCE OF THIS SOFTWARE.

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Exceptions;

with Reqrep_Task_Pools;
use all type Reqrep_Task_Pools.Reqrep_Status;
with Ada.Task_Identification;
with Ada.Task_Termination;

procedure Error_Handling_Example is

   type Example_Reqrep is tagged record
      ID    : Positive := 1;
      D     : Duration := 1.0;
      Fail  : Boolean  := False;
      Crash : Boolean  := False;
      Worker_Crash : Boolean := False;
   end record;

   function Execute
     (R : in out Example_Reqrep) return Reqrep_Task_Pools.Reqrep_Return_Status
   is
   begin
      delay R.D;
      if R.Crash then
         raise Program_Error;
      elsif R.Worker_Crash then
         raise Reqrep_Task_Pools.Inject_Worker_Crash;
      elsif R.Fail then
         return Failure;
      else
         return Success;
      end if;
   end Execute;

   package Example_Task_Pool is new Reqrep_Task_Pools.Task_Pool
     (Reqrep         => Example_Reqrep,
      Number_Workers => 2);
   use Example_Task_Pool;

   Result                         : Reqrep_Job;
   Unhandled_Exception_Occurrence : Ada.Exceptions.Exception_Occurrence;

begin
   Put_Line ("A example of handling errors.");
   New_Line;

   Put_Line ("Pushing requests onto the queue.");

   Example_Task_Pool.Push_Job
     (Example_Reqrep'(ID => 1, D => 1.0, Fail => False, Crash => False, Worker_Crash => False));
   Put_Line ("Request 1 will succeed.");

   Example_Task_Pool.Push_Job
     (Example_Reqrep'(ID => 2, D => 1.0, Fail => True, Crash => False, Worker_Crash => False));
   Put_Line ("Request 2 will fail.");

   Example_Task_Pool.Push_Job
     (Example_Reqrep'(ID => 3, D => 1.0, Fail => False, Crash => True, Worker_Crash => False));
   Put_Line ("Request 3 will crash but the worker should catch this.");

   Example_Task_Pool.Push_Job
     (Example_Reqrep'(ID => 4, D => 5.0, Fail => False, Crash => False, Worker_Crash => False),
      Timeout => 2.0);
   Put_Line ("Request 4 will time out.");

   Example_Task_Pool.Push_Job
     (Example_Reqrep'(ID => 6, D => 1.0, Fail => False, Crash => False, Worker_Crash => True));
   --  (Example_Reqrep'(ID => 5, D => 1.0, Fail => False, Crash => True, Worker_Crash => False));
   Put_Line ("Request 5 will crash the worker.");

   New_Line;

   Put_Line ("Pulling results off the queue.");
   for I in 1 .. 5 loop
      Result := Get_Result;
      Put_Line
        ("Got response: " &
         Reqrep_Task_Pools.Reqrep_Status'Image (Result.Status) &
         " for request ID:" &
         Integer'Image (Result.ID));

      if Result.Status = Unhandled_Exception or Result.Status = Internal_Error then
         Get_Exception (Unhandled_Exception_Occurrence);
         Put_Line
           (Ada.Exceptions.Exception_Information
              (Unhandled_Exception_Occurrence));
      end if;

   end loop;

   Put_Line("Active workers: " & Natural'Image(Active_Workers));
   Put_Line ("Shutting down...");
   Shutdown;
   delay 0.1;
   Put_Line("Active workers: " & Natural'Image(Active_Workers));
   Put_Line ("Shut down.");

end Error_Handling_Example;
