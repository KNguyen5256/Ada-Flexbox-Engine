with Ada.Finalization; use Ada.Finalization;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

with font;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with importer;

with namespaces; use namespaces;

with Widget;
with Widget.Button;

package body dui is

    procedure add_to_LOT (Widget : Any_Acc; Parent : Any_Acc) is
    begin
        dui.LOT.Append_Child
           (dui.Layout_Object_Tree.Find (dui.LOT, Parent), Widget);
    end add_to_LOT;

    procedure draw_image
       (target : in out g.image; img : in out g.image_access; x, y : Natural;
        w, h   :        Natural)
    is
        it, jt : Integer;
    begin
        for j in img'Range (2) loop
            for i in img'Range (1) loop
                it := x + i;
                jt := y + j;
                if it < x + w and jt < y + h then
                    target (it, jt) := img (i, j);
                end if;
            end loop;
        end loop;
    exception
        when others =>
            Put_Line ("ij: " & it'Image & " jt: " & jt'Image);
    end draw_image;

    procedure draw_rect
       (target : in out g.image; x, y : Natural; w, h : Natural; c : g.color)
    is
        Xb     : constant Integer := x;
        Xe     : constant Integer := x + w - 1;
        Yb     : constant Integer := y;
        Ye     : constant Integer := y + h - 1;
        ic, jc : Integer;
    begin
        for I in Xb .. Xe loop
            for J in Yb .. Ye loop
                ic            := I;
                jc            := J;
                target (I, J) := c;
            end loop;
        end loop;
    exception
        when others =>
            Put_Line ("i: " & ic'Image & " J: " & jc'Image);
    end draw_rect;

    procedure draw_character
       (c    : Character; magnification : Natural; target : in out g.image;
        x, y : Natural; color : g.color)
    is
        x_font : constant Integer := font.get_font_char_start (c);
        y_font : constant Integer := 1;
        use g;
    begin
        for fj in font.bitmap_height_t'First .. font.bitmap_base loop
            for fi in font.bitmap_width_t'First .. font.bitmap_base loop
                --Put_Line (font.font_1_img (x_font + fi - 1, y_font + fj - 1)'image);
                --Put_Line (g.color_val'last'image);
                if font.font_1_img (x_font + fi - 1, y_font + fj - 1) = g.white
                then
                    for mj in 1 .. magnification loop
                        for mi in 1 .. magnification loop
                            target
                               (x + (fi * magnification) + mi - 1,
                                y + (fj * magnification) + mj - 1) :=
                               color;
                        end loop;
                    end loop;
                end if;
            end loop;
        end loop;
    end draw_character;

    procedure draw_text
       (target : in out g.image; text : String; magnification : Natural;
        x, y   :        Natural; color : g.color)
    is
        use g;
    begin
        for c in text'Range loop
            declare
                i : Natural :=
                   x + (c - 1) * (magnification * (font.bitmap_base + 1));
                j : Natural := y;
            begin
                draw_character (text (c), magnification, target, i, j, color);
            end;
        end loop;
    exception
        when others =>
            Put_Line ("draw text problem!");
    end draw_text;

    -- need a pass from leaf to root to compute intrinsic, inner content width and height

    procedure render
       (target        : in out graphic.image; window_width : Natural;
        window_height :        Natural)
    is

        procedure render_node (c : Layout_Object_Tree.Cursor) is
        begin
            Layout_Object_Tree.Element (c).Draw (target);
        end render_node;

        procedure test (c : Layout_Object_Tree.Cursor) is
        begin
            if Layout_Object_Tree.Element (c).all in Loadable'Class then
                Loadable'Class (Layout_Object_Tree.Element (c).all).Load;
            end if;
        end test;

        procedure compute_node (c : Layout_Object_Tree.Cursor) is
            cc : Natural := Natural (Layout_Object_Tree.Child_Count (c));
            e            : w.Class := Layout_Object_Tree.Element (c).all; --parent
            cw           : Natural := e.w; --current width
            ch           : Natural := e.h; --current height
            cx           : Natural := e.x; --child x coord
            cy           : Natural := e.y; --child y coord
            counter      : Natural := 0;
            child_row    : Boolean :=
               (e.child_flex.dir = left_right or
                e.child_flex.dir = right_left);
            child_column : Boolean :=
               (e.child_flex.dir = top_bottom or
                e.child_flex.dir = bottom_top);
            child_depth  : Boolean :=
               (e.child_flex.dir = front_back or
                e.child_flex.dir = back_front);
            buoy_w       : buoy_t;
            
            expand_w, expand_h : expand_t;
            width_pixel_left   : Natural := e.w;
            height_pixel_left  : Natural := e.h;
            total_portion      : Natural := 0;
            nbr_max            : Natural := 0;
        
        procedure calculate_portions is
        begin
            for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop
                        if child_row then
                            expand_w := LOT (i).self_flex.expand_w;
                            case expand_w.behavior is
                                when portion =>
                                    total_portion :=
                                       total_portion + expand_w.portion;
                                when pixel =>
                                    width_pixel_left :=
                                       width_pixel_left - expand_w.pixel;
                                when percent =>
                                    width_pixel_left :=
                                       width_pixel_left -
                                       Natural
                                          (percent_t (e.w) * expand_w.percent);
                                when content =>
                                    width_pixel_left :=
                                       width_pixel_left - LOT (i).w;
                                when max =>
                                    nbr_max := nbr_max + 1;
                            end case;
                        elsif child_column then
                            expand_h := LOT (i).self_flex.expand_h;
                            case expand_h.behavior is
                                when portion =>
                                    total_portion :=
                                       total_portion + expand_h.portion;
                                when pixel =>
                                    height_pixel_left :=
                                       height_pixel_left - expand_h.pixel;
                                when percent =>
                                    height_pixel_left :=
                                       height_pixel_left -
                                       Natural
                                          (percent_t (e.h) * expand_h.percent);
                                when content =>
                                    height_pixel_left :=
                                       height_pixel_left - LOT (i).h;
                                when max =>
                                    nbr_max := nbr_max + 1;
                            end case;
                        elsif child_depth then
                            null;
                        end if;
                    end loop;
        end calculate_portions;

        procedure calculate_children_coordinates is
        begin
            for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop
                        if e.child_flex.dir = right_left then
                            LOT(i).x := cw - LOT(i).w + e.x;
                            LOT(i).y := cy;
                        elsif e.child_flex.dir = bottom_top then
                            LOT(i).y := ch - cy - LOT(i).h;
                            LOT(i).x := cx;
                        else
                            LOT (i).x := cx;
                            LOT (i).y := cy;
                        end if;
                        expand_w  := LOT (i).self_flex.expand_w;
                        expand_h  := LOT (i).self_flex.expand_h;
                        if child_row then
                            case expand_w.behavior
                            is -- update w in row context
                                when portion =>
                                    LOT (i).w :=
                                       (e.w / total_portion) *
                                       expand_w.portion;
                                when pixel =>
                                    LOT (i).w := expand_w.pixel;
                                when percent =>
                                    LOT (i).w :=
                                       natural
                                          (percent_t (e.w) * expand_w.percent);
                                when content =>
                                    null;
                                when max =>
                                    LOT (i).w := (width_pixel_left / nbr_max);
                            end case;
                            case expand_h.behavior
                            is  -- update h in row context
                                when portion =>
                                    null;
                                when pixel =>
                                    LOT (i).h := expand_h.pixel;
                                when percent =>
                                    LOT (i).h :=
                                       natural
                                          (percent_t (e.h) * expand_h.percent);
                                when content =>
                                    null;
                                when max =>
                                    LOT (i).h := e.h;
                            end case;
                            
                            cx := cx + LOT(i).w;

                        elsif child_column then
                            case expand_h.behavior
                            is -- update h in column context
                                when portion =>
                                    LOT (i).h :=
                                       (e.h / total_portion) *
                                       expand_h.portion;
                                when pixel =>
                                    LOT (i).h := expand_h.pixel;
                                when percent =>
                                    LOT (i).h :=
                                       natural
                                          (percent_t (e.h) * expand_w.percent);
                                when content =>
                                    null;
                                when max =>
                                    LOT (i).h := (height_pixel_left / nbr_max);
                            end case;
                            case expand_w.behavior
                            is -- update w in column context
                                when portion =>
                                    null;
                                when pixel =>
                                    LOT (i).w := expand_w.pixel;
                                when percent =>
                                    LOT (i).w :=
                                       natural
                                          (percent_t (e.w) * expand_w.percent);
                                when content =>
                                    null;
                                when max =>
                                    LOT (i).w := e.w;
                            end case;

                            cy := cy + LOT(i).h;
                            
                            
                        elsif child_depth then
                            null;
                        end if;
                        counter := counter + 1;
                    end loop;
        end calculate_children_coordinates;

        procedure calculate_buoy is
        rWidth          : Natural := 0;
        cTotalWidth     : Natural := 0;
        spaceBetween    : Natural := 0;
        spaceAround     : Natural := 0;
        spaceEven       : Natural := 0;
        counter         : Natural := 0;
        begin
            buoy_w := e.child_flex.buoy;
            case buoy_w is 
                when space_between =>
                    for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop
                        cTotalWidth := LOT(i).w + cTotalWidth;
                    end loop;
                    spaceBetween := (cw - cTotalWidth)/(cc - 1);
                    for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop
                        if e.child_flex.dir = right_left then
                            if rWidth = 0 then
                                rWidth := LOT(i).x;
                            else
                                LOT(i).x := rWidth - spaceBetween - LOT(i).w;
                                rWidth := LOT(i).x;
                            end if;
                        else
                            if rWidth = 0 then
                                rWidth := LOT(i).w + LOT(i).x;
                            else
                                LOT(i).x := spaceBetween + rWidth;
                                rWidth := LOT(i).w + LOT(i).x;
                            end if;
                        end if;
                    end loop;
                when space_around =>
                    for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop
                        cTotalWidth := LOT(i).w + cTotalWidth;
                    end loop;
                    spaceAround := (cw - cTotalWidth)/(2 * cc);
                    for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop
                        if e.child_flex.dir = right_left then
                            if rWidth = 0 then
                                LOT(i).x := LOT(i).x - spaceAround;
                                rWidth := LOT(i).x;
                            else
                                LOT(i).x := rWidth - 2 * spaceAround - LOT(i).w;
                                rWidth := LOT(i).x;
                            end if;
                        else
                            if rWidth = 0 then
                                LOT(i).x := LOT(i).x + spaceAround;
                                rWidth := LOT(i).w + LOT(i).x;
                            else
                                LOT(i).x := 2 * spaceAround + rWidth;
                                rWidth := LOT(i).w + LOT(i).x;
                            end if;
                        end if;
                    end loop;
                when space_even =>
                    for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop
                        cTotalWidth := LOT(i).w + cTotalWidth;
                    end loop;
                    spaceEven := (cw - cTotalWidth)/(cc + 1);
                    for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop
                        if e.child_flex.dir = right_left then
                            if rWidth = 0 then
                                LOT(i).x := LOT(i).x - spaceEven;
                                rWidth := LOT(i).x;
                            else
                                LOT(i).x := rWidth - spaceEven - LOT(i).w;
                                rWidth := LOT(i).x;
                            end if;
                        else
                            if rWidth = 0 then
                                LOT(i).x := LOT(i).x + spaceEven;
                                rWidth := LOT(i).w + LOT(i).x;
                            else
                                LOT(i).x := spaceEven + rWidth;
                                rWidth := LOT(i).w + LOT(i).x;
                            end if;
                        end if;
                    end loop;
                when space_nothing =>
                    null;
                when others =>
                    null;
            end case;
        end calculate_buoy;

        begin
            if cc > 0 then
                begin
                    calculate_portions; -- Procedure call calculated data necessary to calculate (x,y) coordinates of widgets to be drawn.
                    calculate_children_coordinates; --Procedure call traverses children of current widget to calculate their (x,y) coordinates.
                    if cc > 1 then
                        calculate_buoy; --test
                    end if;
                end;
            end if;
        end compute_node;

        Start_Time   : Time;
        Elapsed_Time : Time_Span;

    begin
        Start_Time                                        := Clock;
        LOT (Layout_Object_Tree.First_Child (LOT.Root)).w := window_width;
        LOT (Layout_Object_Tree.First_Child (LOT.Root)).h := window_height;
        Layout_Object_Tree.Iterate (LOT, compute_node'Access);
        Layout_Object_Tree.Iterate (LOT, render_node'Access);
        --Layout_Object_Tree.Iterate (LOT, test'access);
        Elapsed_Time := Clock - Start_Time;
        -- Put_Line ("Elapsed time (whole dui): "
        --    & Duration'Image (To_Duration (Elapsed_Time))
        --    & " seconds");
    end render;

    

    procedure handle_click_event (x_Input: Natural; y_Input : Natural) is

    procedure click_event
       (c : Layout_Object_Tree.Cursor)
    is
    begin
        if Layout_Object_Tree.Element(c).Is_In_Bound (x_Input, y_Input) then
            Layout_Object_Tree.Element(c).Who_I_Am;
        end if;
    end click_event;

    begin
        Layout_Object_Tree.Iterate (LOT, click_event'Access);
    end handle_click_event;

    procedure handle_release_event is
    procedure release_event(c: Layout_Object_Tree.Cursor) is
    begin
        if Layout_Object_Tree.Element(c).Is_Clickable then
            Widget.Button.Any_Acc(Layout_Object_Tree.Element(c)).release_click;
        end if;
    end;
    begin
        Layout_Object_Tree.Iterate(LOT, release_event'Access);
    end handle_release_event;

begin
    main_widget :=
       new w.Instance'
          (Controlled with id => +"main",
           child_flex => (dir => top_bottom, others => <>), others => <>);
    LOT.Append_Child (Parent => LOT_Root, New_Item => main_widget);
    LOT_Root        := Layout_Object_Tree.First_Child (LOT_Root);
    font.font_1_img := g.Load_QOI ("data/font_1.qoi");
end dui;
