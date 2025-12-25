-- SPDX-License-Identifier: AGPL-3.0-or-later
-- bitfuckit - Bitbucket CLI tool in Ada
-- Usage:
--   bitfuckit auth login
--   bitfuckit repo create <name> [--private] [--description "desc"]
--   bitfuckit repo list
--   bitfuckit repo delete <name>
--   bitfuckit repo exists <name>
--   bitfuckit mirror <github-repo>

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Config;
with Bitbucket_API;
with TUI;
with GNAT.OS_Lib;

procedure Bitfuckit is

   procedure Print_Usage is
   begin
      Put_Line ("bitfuckit - Bitbucket CLI tool in Ada/SPARK");
      Put_Line ("");
      Put_Line ("Usage:");
      Put_Line ("  bitfuckit                         Launch TUI interface");
      Put_Line ("  bitfuckit tui                     Launch TUI interface");
      Put_Line ("  bitfuckit auth login              Login with app password");
      Put_Line ("  bitfuckit auth status             Show auth status");
      Put_Line ("  bitfuckit repo create <name>      Create repository");
      Put_Line ("    --private                       Make repo private");
      Put_Line ("    --description ""text""            Set description");
      Put_Line ("  bitfuckit repo list               List repositories");
      Put_Line ("  bitfuckit repo delete <name>      Delete repository");
      Put_Line ("  bitfuckit repo exists <name>      Check if repo exists");
      Put_Line ("  bitfuckit mirror <name>           Mirror from GitHub");
      Put_Line ("");
      Put_Line ("Get app password: https://bitbucket.org/account/settings/app-passwords/");
   end Print_Usage;

   procedure Do_Auth_Login is
      Username : Unbounded_String;
      Password : Unbounded_String;
      Workspace : Unbounded_String;
      Creds : Config.Credentials;
      Line : String (1 .. 256);
      Last : Natural;
   begin
      Put ("Bitbucket username: ");
      Get_Line (Line, Last);
      Username := To_Unbounded_String (Line (1 .. Last));

      Put ("App password: ");
      Get_Line (Line, Last);
      Password := To_Unbounded_String (Line (1 .. Last));

      Put ("Workspace (usually same as username): ");
      Get_Line (Line, Last);
      Workspace := To_Unbounded_String (Line (1 .. Last));

      if Length (Workspace) = 0 then
         Workspace := Username;
      end if;

      Creds := (Username => Username,
                App_Password => Password,
                Workspace => Workspace);

      Config.Save_Credentials (Creds);
      Put_Line ("Credentials saved to " & Config.Get_Config_File);

      -- Test the credentials
      declare
         Result : constant Bitbucket_API.API_Result :=
            Bitbucket_API.List_Repos (Creds);
      begin
         if Result.Success then
            Put_Line ("Authentication successful!");
         else
            Put_Line ("Warning: Could not verify credentials");
         end if;
      end;
   end Do_Auth_Login;

   procedure Do_Auth_Status is
      Creds : constant Config.Credentials := Config.Load_Credentials;
   begin
      if Config.Has_Credentials then
         Put_Line ("Logged in as: " & To_String (Creds.Username));
         Put_Line ("Workspace: " & To_String (Creds.Workspace));
         Put_Line ("Config: " & Config.Get_Config_File);
      else
         Put_Line ("Not logged in. Run: bitfuckit auth login");
      end if;
   end Do_Auth_Status;

   procedure Do_Repo_Create is
      Creds : constant Config.Credentials := Config.Load_Credentials;
      Name : Unbounded_String := Null_Unbounded_String;
      Description : Unbounded_String := Null_Unbounded_String;
      Is_Private : Boolean := False;
      I : Integer := 3;
   begin
      if not Config.Has_Credentials then
         Put_Line ("Error: Not logged in. Run: bitfuckit auth login");
         Set_Exit_Status (1);
         return;
      end if;

      while I <= Argument_Count loop
         declare
            Arg : constant String := Argument (I);
         begin
            if Arg = "--private" then
               Is_Private := True;
            elsif Arg = "--description" and then I < Argument_Count then
               I := I + 1;
               Description := To_Unbounded_String (Argument (I));
            elsif Length (Name) = 0 then
               Name := To_Unbounded_String (Arg);
            end if;
         end;
         I := I + 1;
      end loop;

      if Length (Name) = 0 then
         Put_Line ("Error: Repository name required");
         Set_Exit_Status (1);
         return;
      end if;

      Put ("Creating repository " & To_String (Name) & "... ");
      declare
         Result : constant Bitbucket_API.API_Result :=
            Bitbucket_API.Create_Repo
              (Creds, To_String (Name), Is_Private, To_String (Description));
      begin
         if Result.Success then
            Put_Line ("done!");
            Put_Line ("https://bitbucket.org/" &
                      To_String (Creds.Workspace) & "/" &
                      To_String (Name));
         else
            Put_Line ("failed!");
            Put_Line (To_String (Result.Message));
            Set_Exit_Status (1);
         end if;
      end;
   end Do_Repo_Create;

   procedure Do_Repo_List is
      Creds : constant Config.Credentials := Config.Load_Credentials;
   begin
      if not Config.Has_Credentials then
         Put_Line ("Error: Not logged in. Run: bitfuckit auth login");
         Set_Exit_Status (1);
         return;
      end if;

      declare
         Result : constant Bitbucket_API.API_Result :=
            Bitbucket_API.List_Repos (Creds);
         Data : constant String := To_String (Result.Data);
         Pos : Natural := 1;
         Slug_Start : Natural;
         Slug_End : Natural;
      begin
         if not Result.Success then
            Put_Line ("Error: " & To_String (Result.Message));
            Set_Exit_Status (1);
            return;
         end if;

         Put_Line ("Repositories in " & To_String (Creds.Workspace) & ":");
         Put_Line ("");

         -- Simple JSON parsing for slugs
         loop
            Slug_Start := Ada.Strings.Unbounded.Index
              (Result.Data, """slug"": """, Pos);
            exit when Slug_Start = 0;

            Slug_Start := Slug_Start + 9;
            Slug_End := Ada.Strings.Unbounded.Index
              (Result.Data, """", Slug_Start);
            exit when Slug_End = 0;

            Put_Line ("  " & Slice (Result.Data, Slug_Start, Slug_End - 1));
            Pos := Slug_End + 1;
         end loop;
      end;
   end Do_Repo_List;

   procedure Do_Repo_Delete is
      Creds : constant Config.Credentials := Config.Load_Credentials;
      Name : constant String := (if Argument_Count >= 3
                                  then Argument (3)
                                  else "");
      Confirm : String (1 .. 10);
      Last : Natural;
   begin
      if not Config.Has_Credentials then
         Put_Line ("Error: Not logged in. Run: bitfuckit auth login");
         Set_Exit_Status (1);
         return;
      end if;

      if Name'Length = 0 then
         Put_Line ("Error: Repository name required");
         Set_Exit_Status (1);
         return;
      end if;

      Put ("Delete repository " & Name & "? Type 'yes' to confirm: ");
      Get_Line (Confirm, Last);

      if Confirm (1 .. Last) /= "yes" then
         Put_Line ("Aborted.");
         return;
      end if;

      declare
         Result : constant Bitbucket_API.API_Result :=
            Bitbucket_API.Delete_Repo (Creds, Name);
      begin
         if Result.Success then
            Put_Line ("Deleted: " & Name);
         else
            Put_Line ("Error: " & To_String (Result.Message));
            Set_Exit_Status (1);
         end if;
      end;
   end Do_Repo_Delete;

   procedure Do_Repo_Exists is
      Creds : constant Config.Credentials := Config.Load_Credentials;
      Name : constant String := (if Argument_Count >= 3
                                  then Argument (3)
                                  else "");
   begin
      if not Config.Has_Credentials then
         Put_Line ("Error: Not logged in. Run: bitfuckit auth login");
         Set_Exit_Status (1);
         return;
      end if;

      if Name'Length = 0 then
         Put_Line ("Error: Repository name required");
         Set_Exit_Status (1);
         return;
      end if;

      if Bitbucket_API.Repo_Exists (Creds, Name) then
         Put_Line ("Repository exists: " & Name);
      else
         Put_Line ("Repository not found: " & Name);
         Set_Exit_Status (1);
      end if;
   end Do_Repo_Exists;

   procedure Do_Mirror is
      Creds : constant Config.Credentials := Config.Load_Credentials;
      Name : constant String := (if Argument_Count >= 2
                                  then Argument (2)
                                  else "");
      Result : Bitbucket_API.API_Result;
      Push_Result : Integer;
      Args : GNAT.OS_Lib.Argument_List_Access;
   begin
      if not Config.Has_Credentials then
         Put_Line ("Error: Not logged in. Run: bitfuckit auth login");
         Set_Exit_Status (1);
         return;
      end if;

      if Name'Length = 0 then
         Put_Line ("Error: Repository name required");
         Put_Line ("Usage: bitfuckit mirror <repo-name>");
         Set_Exit_Status (1);
         return;
      end if;

      -- Create repo if it doesn't exist
      if not Bitbucket_API.Repo_Exists (Creds, Name) then
         Put ("Creating repository " & Name & "... ");
         Result := Bitbucket_API.Create_Repo (Creds, Name, False, "");
         if Result.Success then
            Put_Line ("done!");
         else
            Put_Line ("failed!");
            Set_Exit_Status (1);
            return;
         end if;
      else
         Put_Line ("Repository exists: " & Name);
      end if;

      -- Push to Bitbucket
      Put_Line ("Pushing to Bitbucket...");
      Args := GNAT.OS_Lib.Argument_String_To_List
        ("push --all git@bitbucket.org:" &
         To_String (Creds.Workspace) & "/" & Name & ".git");

      Push_Result := GNAT.OS_Lib.Spawn
        (Program_Name => "/usr/bin/git",
         Args => Args.all);

      if Push_Result = 0 then
         Put_Line ("Mirror complete!");
         Put_Line ("https://bitbucket.org/" &
                   To_String (Creds.Workspace) & "/" & Name);
      else
         Put_Line ("Push failed with exit code:" & Push_Result'Image);
         Set_Exit_Status (1);
      end if;
   end Do_Mirror;

begin
   if Argument_Count = 0 then
      TUI.Run_TUI;
      return;
   end if;

   declare
      Cmd : constant String := Argument (1);
      Sub : constant String := (if Argument_Count >= 2
                                 then Argument (2)
                                 else "");
   begin
      if Cmd = "auth" then
         if Sub = "login" then
            Do_Auth_Login;
         elsif Sub = "status" then
            Do_Auth_Status;
         else
            Put_Line ("Unknown auth command. Use: login, status");
            Set_Exit_Status (1);
         end if;

      elsif Cmd = "repo" then
         if Sub = "create" then
            Do_Repo_Create;
         elsif Sub = "list" then
            Do_Repo_List;
         elsif Sub = "delete" then
            Do_Repo_Delete;
         elsif Sub = "exists" then
            Do_Repo_Exists;
         else
            Put_Line ("Unknown repo command. Use: create, list, delete, exists");
            Set_Exit_Status (1);
         end if;

      elsif Cmd = "mirror" then
         Do_Mirror;

      elsif Cmd = "tui" then
         TUI.Run_TUI;

      elsif Cmd = "help" or else Cmd = "--help" or else Cmd = "-h" then
         Print_Usage;

      else
         Put_Line ("Unknown command: " & Cmd);
         Print_Usage;
         Set_Exit_Status (1);
      end if;
   end;
end Bitfuckit;
