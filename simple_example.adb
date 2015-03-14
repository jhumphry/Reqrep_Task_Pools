-- simple_example.adb
-- A  simple example of using the Reqrep_Task_Pool

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

with Reqrep_Task_Pools;
use all type Reqrep_Task_Pools.Reqrep_Status;

procedure Simple_Example is

   type Delay_Reqrep is tagged record
      ID : Positive := 1;
      D  : Duration := 1.0;
   end record;

   function Execute
     (R : in out Delay_Reqrep) return Reqrep_Task_Pools.Reqrep_Return_Status
   is
   begin
      delay R.D;
      return Success;
   end Execute;

   package Delay_Task_Pool is new Reqrep_Task_Pools.Task_Pool
     (Reqrep         => Delay_Reqrep,
      Number_Workers => 2);

   Result : Delay_Task_Pool.Reqrep_Job;

begin
   Put_Line ("A simple example of Request-Response handling.");
   New_Line;

   Put_Line ("Pushing requests onto the queue.");
   for I in 1 .. 5 loop
      Delay_Task_Pool.Push_Job (Delay_Reqrep'(ID => I, D => 1.0));
      Put_Line ("Pushed request:" & Integer'Image (I));
   end loop;

   New_Line;

   Put_Line ("Pulling results off the queue.");
   for I in 1 .. 5 loop
      Result := Delay_Task_Pool.Get_Result;
      Put_Line
        ("Got response: " &
         Reqrep_Task_Pools.Reqrep_Status'Image (Result.Status) &
         " for request ID:" &
         Integer'Image (Result.ID));
   end loop;

   Delay_Task_Pool.Shutdown;

end Simple_Example;
