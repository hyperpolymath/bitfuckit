-- SPDX-License-Identifier: PMPL-1.0

with GNAT.OS_Lib;
with GNAT.Expect;
with Ada.Text_IO;

package body Bitbucket_API is

   Base_URL : constant String := "https://api.bitbucket.org/2.0";

   function Run_Curl
     (Args : String) return API_Result
   is
      use GNAT.Expect;
      Pd : Process_Descriptor;
      Result : Expect_Match;
      Output : Unbounded_String := Null_Unbounded_String;
      Ret : API_Result;
   begin
      Non_Blocking_Spawn
        (Pd,
         "/usr/bin/curl",
         GNAT.OS_Lib.Argument_String_To_List (Args).all,
         Err_To_Out => True);

      loop
         begin
            Expect (Pd, Result, ".+", Timeout => 30_000);
            Append (Output, Expect_Out (Pd));
         exception
            when Process_Died =>
               exit;
         end;
      end loop;

      Close (Pd);

      Ret.Data := Output;

      if Index (Output, """error""") > 0
      then
         Ret.Success := False;
         Ret.Message := To_Unbounded_String ("API error");
      else
         Ret.Success := True;
         Ret.Message := To_Unbounded_String ("OK");
      end if;

      return Ret;

   exception
      when others =>
         Ret.Success := False;
         Ret.Message := To_Unbounded_String ("Failed to execute curl");
         return Ret;
   end Run_Curl;

   function Create_Repo
     (Creds : Config.Credentials;
      Name : String;
      Is_Private : Boolean := False;
      Description : String := "") return API_Result
   is
      URL : constant String :=
         Base_URL & "/repositories/" &
         To_String (Creds.Workspace) & "/" & Name;
      Private_Str : constant String :=
         (if Is_Private then "true" else "false");
      Data : constant String :=
         "{""scm"":""git"",""is_private"":" & Private_Str &
         ",""description"":""" & Description & """}";
      Args : constant String :=
         "-s -X POST " &
         "-u " & To_String (Creds.Username) & ":" &
         To_String (Creds.App_Password) & " " &
         "-H ""Content-Type: application/json"" " &
         "-d '" & Data & "' " &
         URL;
   begin
      return Run_Curl (Args);
   end Create_Repo;

   function Delete_Repo
     (Creds : Config.Credentials;
      Name : String) return API_Result
   is
      URL : constant String :=
         Base_URL & "/repositories/" &
         To_String (Creds.Workspace) & "/" & Name;
      Args : constant String :=
         "-s -X DELETE " &
         "-u " & To_String (Creds.Username) & ":" &
         To_String (Creds.App_Password) & " " &
         URL;
   begin
      return Run_Curl (Args);
   end Delete_Repo;

   function List_Repos
     (Creds : Config.Credentials) return API_Result
   is
      URL : constant String :=
         Base_URL & "/repositories/" &
         To_String (Creds.Workspace) & "?pagelen=100";
      Args : constant String :=
         "-s -X GET " &
         "-u " & To_String (Creds.Username) & ":" &
         To_String (Creds.App_Password) & " " &
         URL;
   begin
      return Run_Curl (Args);
   end List_Repos;

   function Get_Repo
     (Creds : Config.Credentials;
      Name : String) return API_Result
   is
      URL : constant String :=
         Base_URL & "/repositories/" &
         To_String (Creds.Workspace) & "/" & Name;
      Args : constant String :=
         "-s -X GET " &
         "-u " & To_String (Creds.Username) & ":" &
         To_String (Creds.App_Password) & " " &
         URL;
   begin
      return Run_Curl (Args);
   end Get_Repo;

   function Repo_Exists
     (Creds : Config.Credentials;
      Name : String) return Boolean
   is
      Result : constant API_Result := Get_Repo (Creds, Name);
   begin
      return Result.Success and then
         Index (Result.Data, """slug"":""" & Name & """") > 0;
   end Repo_Exists;

   function List_Pull_Requests
     (Creds : Config.Credentials;
      Repo_Name : String;
      State : String := "OPEN") return API_Result
   is
      State_Param : constant String :=
         (if State'Length > 0
          then "&state=" & State
          else "");
      URL : constant String :=
         Base_URL & "/repositories/" &
         To_String (Creds.Workspace) & "/" & Repo_Name &
         "/pullrequests?pagelen=50" & State_Param;
      Args : constant String :=
         "-s -X GET " &
         "-u " & To_String (Creds.Username) & ":" &
         To_String (Creds.App_Password) & " " &
         URL;
   begin
      return Run_Curl (Args);
   end List_Pull_Requests;

end Bitbucket_API;
