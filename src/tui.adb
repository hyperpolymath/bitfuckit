-- SPDX-License-Identifier: AGPL-3.0-or-later

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

package body TUI
   with SPARK_Mode => Off  -- I/O operations
is
   package Latin renames Ada.Characters.Latin_1;

   ESC : constant Character := Latin.ESC;

   function Color_Code (C : Color) return String is
   begin
      case C is
         when Default => return "0";
         when Red     => return "31";
         when Green   => return "32";
         when Yellow  => return "33";
         when Blue    => return "34";
         when Magenta => return "35";
         when Cyan    => return "36";
         when White   => return "37";
      end case;
   end Color_Code;

   procedure Clear_Screen is
   begin
      Put (ESC & "[2J" & ESC & "[H");
   end Clear_Screen;

   procedure Move_Cursor (Row, Col : Positive) is
      Row_Str : constant String := Positive'Image (Row);
      Col_Str : constant String := Positive'Image (Col);
   begin
      Put (ESC & "[" & Row_Str (2 .. Row_Str'Last) & ";" &
           Col_Str (2 .. Col_Str'Last) & "H");
   end Move_Cursor;

   procedure Set_Color (Fg : Color; Bold : Boolean := False) is
      Bold_Str : constant String := (if Bold then "1;" else "");
   begin
      Put (ESC & "[" & Bold_Str & Color_Code (Fg) & "m");
   end Set_Color;

   procedure Reset_Color is
   begin
      Put (ESC & "[0m");
   end Reset_Color;

   procedure Put_Colored (Text : String; Fg : Color; Bold : Boolean := False) is
   begin
      Set_Color (Fg, Bold);
      Put (Text);
      Reset_Color;
   end Put_Colored;

   procedure Put_Line_Colored (Text : String; Fg : Color; Bold : Boolean := False) is
   begin
      Set_Color (Fg, Bold);
      Put_Line (Text);
      Reset_Color;
   end Put_Line_Colored;

   procedure Draw_Header (Title : String) is
      Line : constant String (1 .. 60) := (others => '═');
   begin
      Move_Cursor (1, 1);
      Put_Colored ("╔" & Line & "╗", Cyan, True);
      Move_Cursor (2, 1);
      Put_Colored ("║", Cyan, True);
      Set_Color (Yellow, True);
      Put ("  🪣 " & Title);
      Move_Cursor (2, 62);
      Put_Colored ("║", Cyan, True);
      Move_Cursor (3, 1);
      Put_Colored ("╚" & Line & "╝", Cyan, True);
   end Draw_Header;

   procedure Draw_Footer (Status : String) is
   begin
      Move_Cursor (24, 1);
      Put_Colored ("─────────────────────────────────────────────────────────────", Blue);
      Move_Cursor (25, 1);
      Put_Colored (Status, White);
   end Draw_Footer;

   procedure Draw_Box (Row, Col, Width, Height : Positive; Title : String := "") is
      Top_Line    : constant String (1 .. Width - 2) := (others => '─');
      Bottom_Line : constant String (1 .. Width - 2) := (others => '─');
      Empty_Line  : constant String (1 .. Width - 2) := (others => ' ');
   begin
      Move_Cursor (Row, Col);
      Put ("┌" & Top_Line & "┐");

      if Title'Length > 0 then
         Move_Cursor (Row, Col + 2);
         Put_Colored (" " & Title & " ", Yellow, True);
      end if;

      for I in 1 .. Height - 2 loop
         Move_Cursor (Row + I, Col);
         Put ("│" & Empty_Line & "│");
      end loop;

      Move_Cursor (Row + Height - 1, Col);
      Put ("└" & Bottom_Line & "┘");
   end Draw_Box;

   procedure Draw_Menu (Items : Menu_Items; Selected : Positive) is
   begin
      for I in Items'Range loop
         Move_Cursor (5 + I, 4);
         if I = Selected then
            Put_Colored ("► ", Green, True);
            Put_Colored ("[" & Items (I).Key & "] ", Yellow, True);
            Put_Colored (To_String (Items (I).Label), White, True);
         else
            Put ("  ");
            Put_Colored ("[" & Items (I).Key & "] ", Cyan);
            Put (To_String (Items (I).Label));
         end if;
         Put (" - ");
         Put_Colored (To_String (Items (I).Description), Blue);
      end loop;
   end Draw_Menu;

   procedure Draw_Progress (Current, Total : Natural; Label : String) is
      Width    : constant := 40;
      Filled   : Natural;
      Bar      : String (1 .. Width);
   begin
      if Total > 0 then
         Filled := (Current * Width) / Total;
      else
         Filled := 0;
      end if;

      for I in 1 .. Width loop
         if I <= Filled then
            Bar (I) := '█';
         else
            Bar (I) := '░';
         end if;
      end loop;

      Move_Cursor (20, 4);
      Put_Colored (Label & ": ", White);
      Put_Colored ("[", Cyan);
      Put_Colored (Bar (1 .. Filled), Green);
      Put (Bar (Filled + 1 .. Width));
      Put_Colored ("]", Cyan);
      Put_Colored (Natural'Image (Current) & "/" & Natural'Image (Total), Yellow);
   end Draw_Progress;

   function Get_Key return Character is
      C : Character;
   begin
      Get_Immediate (C);
      return C;
   end Get_Key;

   function Get_Line_Input (Prompt : String) return Unbounded_String is
      Line : String (1 .. 256);
      Last : Natural;
   begin
      Put_Colored (Prompt, Cyan);
      Put (": ");
      Get_Line (Line, Last);
      return To_Unbounded_String (Line (1 .. Last));
   end Get_Line_Input;

   function Confirm (Prompt : String) return Boolean is
      C : Character;
   begin
      Put_Colored (Prompt & " [y/N]: ", Yellow);
      Get_Immediate (C);
      New_Line;
      return C = 'y' or C = 'Y';
   end Confirm;

   procedure Run_TUI is
      Main_Menu : constant Menu_Items := (
         1 => (Key => 'l', Label => To_Unbounded_String ("Login"),
               Description => To_Unbounded_String ("Authenticate with Bitbucket")),
         2 => (Key => 's', Label => To_Unbounded_String ("Status"),
               Description => To_Unbounded_String ("Show auth status")),
         3 => (Key => 'c', Label => To_Unbounded_String ("Create"),
               Description => To_Unbounded_String ("Create a new repository")),
         4 => (Key => 'r', Label => To_Unbounded_String ("Repos"),
               Description => To_Unbounded_String ("List all repositories")),
         5 => (Key => 'm', Label => To_Unbounded_String ("Mirror"),
               Description => To_Unbounded_String ("Mirror from GitHub")),
         6 => (Key => 'd', Label => To_Unbounded_String ("Delete"),
               Description => To_Unbounded_String ("Delete a repository")),
         7 => (Key => 'q', Label => To_Unbounded_String ("Quit"),
               Description => To_Unbounded_String ("Exit bitfuckit"))
      );
      Selected : Positive := 1;
      Key : Character;
      Running : Boolean := True;
   begin
      while Running loop
         Clear_Screen;
         Draw_Header ("BITFUCKIT - Bitbucket CLI");
         Draw_Box (4, 2, 58, 12, "Main Menu");
         Draw_Menu (Main_Menu, Selected);
         Draw_Footer ("↑/↓ Navigate | Enter Select | q Quit");

         Key := Get_Key;

         case Key is
            when 'k' | 'A' =>  -- Up arrow (A is part of escape sequence)
               if Selected > 1 then
                  Selected := Selected - 1;
               end if;
            when 'j' | 'B' =>  -- Down arrow
               if Selected < Main_Menu'Length then
                  Selected := Selected + 1;
               end if;
            when Latin.LF | Latin.CR =>  -- Enter
               case Selected is
                  when 1 => null; -- Login
                  when 2 => null; -- Status
                  when 3 => null; -- Create
                  when 4 => null; -- List
                  when 5 => null; -- Mirror
                  when 6 => null; -- Delete
                  when 7 => Running := False;
                  when others => null;
               end case;
            when 'l' => null; -- Login shortcut
            when 's' => null; -- Status shortcut
            when 'c' => null; -- Create shortcut
            when 'r' => null; -- Repos shortcut
            when 'm' => null; -- Mirror shortcut
            when 'd' => null; -- Delete shortcut
            when 'q' | Latin.ESC =>
               Running := False;
            when others =>
               null;
         end case;
      end loop;

      Clear_Screen;
      Put_Line_Colored ("Goodbye! 👋", Cyan, True);
   end Run_TUI;

end TUI;
