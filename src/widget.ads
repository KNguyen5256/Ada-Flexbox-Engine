with graphic;
with Ada.Strings.Unbounded;

with Ada.Finalization;

with Ada.Containers.Multiway_Trees;

package Widget is
    package SU renames Ada.Strings.Unbounded;

    function "+" (Source : in String) return SU.Unbounded_String is
        (SU.To_Unbounded_String (source));
    
    function "+" (Source : in SU.Unbounded_String) return String is
        (SU.To_String (source));

   type percent_t is new Float range 0.0 .. 1.0;

   type dir_t      is (left_right, right_left, top_bottom, bottom_top, front_back, back_front);
   type align_t    is (top, right, bottom, left, center);
   type buoy_t     is (space_between, space_around, space_even, space_nothing);
   type behavior_t is (content, portion, pixel, percent, max);

   type expand_t (behavior : behavior_t := max) is record
      case behavior is
         when content =>
            content : Positive;
         when portion =>
            portion : Positive;
         when pixel =>
            pixel : Positive;
         when percent =>
            percent : percent_t;
         when others =>
            null;
      end case;
   end record;

   type gap_t (behavior : behavior_t := max) is record
      case behavior is
         when pixel =>
            pixel : Natural;
         when percent =>
            percent : percent_t;
         when others =>
            null;
      end case;
   end record;

   type flex_t is record
      dir      : dir_t    := left_right;
      align    : align_t  := top;
      buoy     : buoy_t   := space_nothing;
      expand_h : expand_t := (behavior => max);
      expand_w : expand_t := (behavior => max);
      gap_r : gap_t := (behavior => max);
      gap_c : gap_t := (behavior => max);
   end record;

   default_flex : flex_t := (others => <>);

   type Event_Kind is (Click_In, Click_Out, Hover);

   type Instance is new Ada.Finalization.Controlled with 
   record
      id         : SU.Unbounded_String;
      x, y       : Natural   := 0;
      w, h       : Natural   := 50;
      self_flex  : flex_t;
      child_flex : flex_t;
      bgd        : graphic.color := (0.3, 0.3, 0.3, 0.0);
   end record;
   subtype Class is Instance'Class;

   type Acc is access all Instance;
   type Any_Acc is access all Class;

   

   function Create (id         : string;
                    parent     : Widget.Any_Acc;
                    self_flex  : flex_t  := default_flex;
                    child_flex : flex_t  := default_flex;
                    bgd        : graphic.color) return Widget.Any_Acc;

   -- procedure initialize (This : in out Instance);
   -- procedure adjust (This : in out Instance);
   -- procedure finalize (This : in out Instance);
   function Is_In_Bound (This : in out Instance; x_Input: Natural; y_Input : Natural) return Boolean;
   procedure Event (This : in out Instance; Evt : Event_Kind);
   procedure Draw (This : in out Instance; img : in out graphic.image);
   procedure Who_I_Am (This : in out Instance);
   function Is_Clickable(This: in Instance) return Boolean;

private

   

end Widget;
