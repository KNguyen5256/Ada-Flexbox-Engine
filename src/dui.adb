with Ada.Finalization; use Ada.Finalization;
with Ada.Real_Time;    use Ada.Real_Time;
with Ada.Text_IO;      use Ada.Text_IO;

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
                if it <= x + w and jt <= y + h then
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
            LOT_Parent            : w.Class := Layout_Object_Tree.Element (c).all; --parent
            LOT_Parent_Width           : Natural := LOT_Parent.w; --current width
            LOT_Parent_Height           : Natural := LOT_Parent.h; --current height
            LOT_Offset_X           : Natural := LOT_Parent.x; --child x coord
            LOT_Offset_Y           : Natural := LOT_Parent.y; --child y coord
            child_row    : Boolean :=
               (LOT_Parent.child_flex.dir = left_right or
                LOT_Parent.child_flex.dir = right_left);
            child_column      : Boolean :=
               (LOT_Parent.child_flex.dir = top_bottom or
                LOT_Parent.child_flex.dir = bottom_top);
            child_depth       : Boolean :=
               (LOT_Parent.child_flex.dir = front_back or
                LOT_Parent.child_flex.dir = back_front);
            buoy_wh            : buoy_t;
            align_w           : align_t;
gap_r, gap_c : gap_t;
            expand_w, expand_h : expand_t;
            width_pixel_left   : Natural := LOT_Parent.w;
            height_pixel_left  : Natural := LOT_Parent.h;
            total_portion      : Natural := 0;
            nbr_max            : Natural := 0;
            counter            : Natural := 0;
        
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
                                          (percent_t (LOT_Parent.w) * expand_w.percent);
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
                                          (percent_t (LOT_Parent.h) * expand_h.percent);
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
                        if LOT_Parent.child_flex.dir = right_left then
                            LOT(i).x := LOT_Parent_Width - LOT_Offset_X - LOT(i).w + 2 * LOT_Parent.x;
                            LOT(i).y := LOT_Offset_Y;
                        elsif LOT_Parent.child_flex.dir = bottom_top then
                            LOT(i).y := LOT_Parent_Height - LOT_Offset_Y - LOT(i).h + 2 * LOT_Parent.y;
                            LOT(i).x := LOT_Offset_X;
                        else
                            LOT (i).x := LOT_Offset_X;
                            LOT (i).y := LOT_Offset_Y;
                        end if;
                        expand_w  := LOT (i).self_flex.expand_w;
                        expand_h  := LOT (i).self_flex.expand_h;
                        if child_row then
                            case expand_w.behavior
                            is -- update w in row context
                                when portion =>
                                    LOT (i).all.Set_Width((LOT_Parent.w / total_portion) *
                                       expand_w.portion);
                                when pixel =>
                                    LOT (i).all.Set_Width(expand_w.pixel);
                                when percent =>
                                    LOT (i).all.Set_Width(
                                       natural
                                          (percent_t (LOT_Parent.w) * expand_w.percent));
                                when content =>
                                    null;
                                when max =>
                                    LOT (i).all.Set_Width(width_pixel_left / nbr_max);
                            end case;
                            case expand_h.behavior
                            is  -- update h in row context
                                when portion =>
                                    null;
                                when pixel =>
                                    LOT (i).all.Set_Height(expand_h.pixel);
                                when percent =>
                                    LOT (i).all.Set_Height(
                                       natural
                                          (percent_t (LOT_Parent.h) * expand_h.percent));
                                when content =>
                                    null;
                                when max =>
                                    LOT (i).all.Set_Height(LOT_Parent.h);
                            end case;
                            
                            LOT_Offset_X := LOT_Offset_X + LOT(i).w;

                    elsif child_column then
                        case expand_h.behavior is -- update h in column context
                            when portion =>
                                LOT (i).all.Set_Height
                                   ((LOT_Parent.h / total_portion) *
                                    expand_h.portion);
                            when pixel =>
                                LOT (i).all.Set_Height (expand_h.pixel);
                            when percent =>
                                LOT (i).all.Set_Height
                                   (natural
                                       (percent_t (LOT_Parent.h) *
                                        expand_w.percent));
                            when content =>
                                null;
                            when max =>
                                LOT (i).all.Set_Height
                                   (height_pixel_left / nbr_max);
                        end case;
                        case expand_w.behavior is -- update w in column context
                            when portion =>
                                null;
                            when pixel =>
                                LOT (i).all.Set_Width (expand_w.pixel);
                            when percent =>
                                LOT (i).all.Set_Width
                                   (natural
                                       (percent_t (LOT_Parent.w) *
                                        expand_w.percent));
                            when content =>
                                null;
                            when max =>
                                LOT (i).all.Set_Width (LOT_Parent.w);
                        end case;

                            LOT_Offset_Y := LOT_Offset_Y + LOT(i).h;
                            
                            
                        elsif child_depth then
                            null;
                        end if;
                        counter := counter + 1;
                    end loop;
        end calculate_children_coordinates;

        procedure calculate_buoy is
        rWidth          : Natural := 0;
        rHeight         : Natural := 0;
        cTotalWidth     : Natural := 0;
        cTotalHeight    : Natural := 0;
        spaceBetweenX   : Natural := 0;
        spaceBetweenY   : Natural := 0;
        spaceAroundX    : Natural := 0;
        spaceAroundY    : Natural := 0;
        spaceEvenX      : Natural := 0;
        spaceEvenY      : Natural := 0;
        counter         : Natural := 0;
        begin
            buoy_wh := LOT_Parent.child_flex.buoy;
            case buoy_wh is 
                when space_between =>
                    for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop
                        cTotalWidth  := LOT(i).w + cTotalWidth;
                        cTotalHeight := LOT(i).h + cTotalHeight;
                    end loop;
                    spaceBetweenX := (LOT_Parent_Width - cTotalWidth)/(cc - 1);
                    spaceBetweenY := (LOT_Parent_Height - cTotalHeight)/(cc - 1);
                    for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop
                        if LOT_Parent.child_flex.dir = right_left then
                            if rWidth = 0 then
                                rWidth := LOT(i).x;
                            else
                                LOT(i).x := rWidth - spaceBetweenX - LOT(i).w;
                                rWidth := LOT(i).x;
                            end if;
                        elsif LOT_Parent.child_flex.dir = left_right then
                            if rWidth = 0 then
                                rWidth := LOT(i).w + LOT(i).x;
                            else
                                LOT(i).x := spaceBetweenX + rWidth;
                                rWidth := LOT(i).w + LOT(i).x;
                            end if;
                        else
                            null;
                        end if;
                        if LOT_Parent.child_flex.dir = bottom_top then
                            if rHeight = 0 then
                                rHeight := LOT(i).y;
                            else
                                LOT(i).y := rHeight - spaceBetweenY -LOT(i).h;
                                rHeight := LOT(i).y;
                            end if;
                        elsif LOT_Parent.child_flex.dir = top_bottom then
                            if rHeight = 0 then
                                rHeight := LOT(i).h + LOT(i).y;
                            else
                                LOT(i).y := spaceBetweenY + rHeight;
                                rHeight := LOT(i).h + LOT(i).y;
                            end if;
                        else
                            null;
                        end if;
                    end loop;
                when space_around =>
                    for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop
                        cTotalWidth  := LOT(i).w + cTotalWidth;
                        cTotalHeight := LOT(i).h + cTotalHeight;
                    end loop;
                    spaceAroundX := (LOT_Parent_Width - cTotalWidth)/(2 * cc);
                    spaceAroundY := (LOT_Parent_Height - cTotalHeight)/(2 * cc);
                    for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop
                        if LOT_Parent.child_flex.dir = right_left then
                            if rWidth = 0 then
                                LOT(i).x := LOT(i).x - spaceAroundX;
                                rWidth := LOT(i).x;
                            else
                                LOT(i).x := rWidth - 2 * spaceAroundX - LOT(i).w;
                                rWidth := LOT(i).x;
                            end if;
                        elsif LOT_Parent.child_flex.dir = left_right then
                            if rWidth = 0 then
                                LOT(i).x := LOT(i).x + spaceAroundX;
                                rWidth := LOT(i).w + LOT(i).x;
                            else
                                LOT(i).x := 2 * spaceAroundX + rWidth;
                                rWidth := LOT(i).w + LOT(i).x;
                            end if;
                        else
                            null;                           
                        end if;
                        if LOT_Parent.child_flex.dir = bottom_top then
                            if rHeight = 0 then
                                LOT(i).y := LOT(i).y - spaceAroundY;
                                rHeight := LOT(i).y;
                            else
                                LOT(i).y := rHeight - 2 * spaceAroundY - LOT(i).h;
                                rHeight := LOT(i).y;
                            end if;
                        elsif LOT_Parent.child_flex.dir = top_bottom then
                            if rHeight = 0 then
                                LOT(i).y := LOT(i).y + spaceAroundY;
                                rHeight := LOT(i).h + LOT(i).y;
                            else
                                LOT(i).y := 2 * spaceAroundY + rHeight;
                                rHeight := LOT(i).h + LOT(i).y;
                            end if;
                        else
                            null;
                        end if;
                    end loop;
                when space_even =>
                    for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop
                        cTotalWidth  := LOT(i).w + cTotalWidth;
                        cTotalHeight := LOT(i).h + cTotalHeight;
                    end loop;
                    spaceEvenX := (LOT_Parent_Width - cTotalWidth)/(cc + 1);
                    spaceEvenY := (LOT_Parent_Height - cTotalHeight)/(cc + 1);
                    for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop
                        if LOT_Parent.child_flex.dir = right_left then
                            if rWidth = 0 then
                                LOT(i).x := LOT(i).x - spaceEvenX;
                                rWidth := LOT(i).x;
                            else
                                LOT(i).x := rWidth - spaceEvenX - LOT(i).w;
                                rWidth := LOT(i).x;
                            end if;
                        elsif LOT_Parent.child_flex.dir = left_right then
                            if rWidth = 0 then
                                LOT(i).x := LOT(i).x + spaceEvenX;
                                rWidth := LOT(i).w + LOT(i).x;
                            else
                                LOT(i).x := spaceEvenX + rWidth;
                                rWidth := LOT(i).w + LOT(i).x;
                            end if;
                        else
                            null;
                        end if;
                        if LOT_Parent.child_flex.dir = bottom_top then
                            if rHeight = 0 then
                                LOT(i).y := LOT(i).y - spaceEvenY;
                                rHeight := LOT(i).y;
                            else
                                LOT(i).y := rHeight - spaceEvenY - LOT(i).h;
                                rHeight := LOT(i).y;
                            end if;
                        elsif LOT_Parent.child_flex.dir = top_bottom then
                            if rHeight = 0 then
                                LOT(i).y := LOT(i).y + spaceEvenY;
                                rHeight := LOT(i).h + LOT(i).y;
                            else
                                LOT(i).y := spaceEvenY + rHeight;
                                rHeight := LOT(i).h + LOT(i).y;
                            end if;
                        else
                            null;
                        end if;
                    end loop;
                when space_nothing =>
                    null;
                when others =>
                    null;
            end case;
        end calculate_buoy;

        procedure calculate_gap is
        next_sib : Layout_Object_Tree.Cursor;
        prev_sib : Layout_Object_Tree.Cursor;
        modulo_check : Natural;
        midpoint : Natural;
        curr_gap : Natural;
        new_gap : Natural;
        counter : Natural := 1;
        begin
            for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop
                if child_row then
                    gap_r := LOT_Parent.child_flex.gap_r;
                    case gap_r.behavior is
                        when pixel =>
                            new_gap := gap_r.pixel;
                        when percent =>
                            new_gap := natural (percent_t (LOT_Parent.w) * gap_r.percent);
                        when others =>
                            counter := 0;
                    end case;
                    if counter > 0 then
                        if counter > cc then
                            counter := 1;
                        end if;
                        modulo_check := cc mod 2;
                        midpoint := cc / 2 + modulo_check;
                        next_sib := Layout_Object_Tree.Next_Sibling(i);
                        prev_sib := Layout_Object_Tree.Previous_Sibling(i);
                        if LOT_Parent.child_flex.dir = left_right then
                            if LOT_Parent.child_flex.buoy = space_between then -- image compression not implemented yet; doesn't overlap (under?)
                                if Layout_Object_Tree.Has_Element(next_sib) then
                                    curr_gap := LOT(next_sib).x - LOT(i).x - LOT(i).w;
                                    if curr_gap < new_gap then
                                        LOT(next_sib).x := LOT(next_sib).x + new_gap - curr_gap;
                                    end if;
                                end if;
                            elsif LOT_Parent.child_flex.buoy = space_around or LOT_Parent.child_flex.buoy = space_even then
                                if modulo_check = 0 then
                                    if counter < midpoint then
                                        LOT(i).x := LOT(i).x - (midpoint - counter) * new_gap - new_gap mod 2 - new_gap / 2;
                                    elsif counter > midpoint then
                                        LOT(i).x := LOT(i).x + (counter - midpoint - 1) * new_gap + new_gap / 2;
                                    else
                                        LOT(i).x := LOT(i).x - new_gap mod 2 - new_gap / 2;
                                    end if;
                                elsif modulo_check = 1 then
                                    if counter < midpoint then
                                        LOT(i).x := LOT(i).x - (midpoint - counter) * new_gap;
                                    elsif counter > midpoint then
                                        LOT(i).x := LOT(i).x + (counter - midpoint) * new_gap;
                                    end if;
                                end if;
                            else
                                if Layout_Object_Tree.Has_Element(next_sib) then
                                    LOT(next_sib).x := LOT(i).x + LOT(i).w + new_gap;
                                end if;
                            end if;
                        else
                            if LOT_Parent.child_flex.buoy = space_between then -- image compression not implemented yet; causes overlap
                                if Layout_Object_Tree.Has_Element(prev_sib) then
                                    curr_gap := LOT(prev_sib).x - LOT(i).x - LOT(i).w;
                                    if curr_gap < new_gap then
                                        LOT(i).x := LOT(i).x - new_gap + curr_gap;
                                    end if;
                                end if;
                            elsif LOT_Parent.child_flex.buoy = space_around or LOT_Parent.child_flex.buoy = space_even then
                                if modulo_check = 0 then
                                    if counter < midpoint then
                                        LOT(i).x := LOT(i).x + (midpoint - counter) * new_gap + new_gap mod 2 + new_gap / 2;
                                    elsif counter > midpoint then
                                        LOT(i).x := LOT(i).x - (counter - midpoint - 1) * new_gap - new_gap / 2;
                                    else
                                        LOT(i).x := LOT(i).x + new_gap mod 2 + new_gap / 2;
                                    end if;
                                elsif modulo_check = 1 then
                                    if counter < midpoint then
                                        LOT(i).x := LOT(i).x + (midpoint - counter) * new_gap;
                                    elsif counter > midpoint then
                                        LOT(i).x := LOT(i).x - (counter - midpoint) * new_gap;
                                    end if;
                                end if;
                            else
                                if Layout_Object_Tree.Has_Element(prev_sib) then
                                    LOT(i).x := LOT(prev_sib).x - LOT(i).w - new_gap;
                                end if;
                            end if;
                        end if;
                    end if;
                elsif child_column then
                    gap_c := LOT_Parent.child_flex.gap_c;
                    case gap_c.behavior is
                        when pixel =>
                            new_gap := gap_c.pixel;
                        when percent =>
                            new_gap := natural (percent_t (LOT_Parent.h) * gap_c.percent);
                        when others =>
                            counter := 0;
                    end case;
                    if counter > 0 then
                        if counter > cc then
                            counter := 1;
                        end if;
                        modulo_check := cc mod 2;
                        midpoint := cc / 2 + modulo_check;
                        next_sib := Layout_Object_Tree.Next_Sibling(i);
                        prev_sib := Layout_Object_Tree.Previous_Sibling(i);
                        if LOT_Parent.child_flex.dir = top_bottom then
                            if LOT_Parent.child_flex.buoy = space_between then
                                if Layout_Object_Tree.Has_Element(next_sib) then
                                    curr_gap := LOT(next_sib).y - LOT(i).y - LOT(i).h;
                                    if curr_gap < new_gap then
                                        LOT(next_sib).y := LOT(next_sib).y + new_gap - curr_gap;
                                    end if;
                                end if;
                            elsif LOT_Parent.child_flex.buoy = space_around or LOT_Parent.child_flex.buoy = space_even then
                                if modulo_check = 0 then
                                    if counter < midpoint then
                                        LOT(i).y := LOT(i).y - (midpoint - counter) * new_gap - new_gap mod 2 - new_gap / 2;
                                    elsif counter > midpoint then
                                        LOT(i).y := LOT(i).y + (counter - midpoint - 1) * new_gap + new_gap / 2;
                                    else
                                        LOT(i).y := LOT(i).y - new_gap mod 2 - new_gap / 2;
                                    end if;
                                elsif modulo_check = 1 then
                                    if counter < midpoint then
                                        LOT(i).y := LOT(i).y - (midpoint - counter) * new_gap;
                                    elsif counter > midpoint then
                                        LOT(i).y := LOT(i).y + (counter - midpoint) * new_gap;
                                    end if;
                                end if;
                            else
                                if Layout_Object_Tree.Has_Element(next_sib) then
                                    LOT(next_sib).y := LOT(i).y + LOT(i).h + new_gap;
                                end if;
                            end if;
                        else
                            if LOT_Parent.child_flex.buoy = space_between then
                                if Layout_Object_Tree.Has_Element(prev_sib) then
                                    curr_gap := LOT(prev_sib).y - LOT(i).y- LOT(i).h;
                                    if curr_gap < new_gap then
                                        LOT(i).y := LOT(i).y - new_gap + curr_gap;
                                    end if;
                                end if;
                            elsif LOT_Parent.child_flex.buoy = space_around or LOT_Parent.child_flex.buoy = space_even then
                                if modulo_check = 0 then
                                    if counter < midpoint then
                                        LOT(i).y := LOT(i).y + (midpoint - counter) * new_gap + new_gap mod 2 + new_gap / 2;
                                    elsif counter > midpoint then
                                        LOT(i).y := LOT(i).y - (counter - midpoint - 1) * new_gap - new_gap / 2;
                                    else
                                        LOT(i).y := LOT(i).y + new_gap mod 2 + new_gap / 2;
                                    end if;
                                elsif modulo_check = 1 then
                                    if counter < midpoint then
                                        LOT(i).y := LOT(i).y + (midpoint - counter) * new_gap;
                                    elsif counter > midpoint then
                                        LOT(i).y := LOT(i).y - (counter - midpoint) * new_gap;
                                    end if;
                                end if;
                            else
                                if Layout_Object_Tree.Has_Element(prev_sib) then
                                    LOT(i).y := LOT(prev_sib).y - LOT(i).h - new_gap;
                                end if;
                            end if;
                        end if;
                    end if;
                end if;
                counter := counter + 1;
            end loop;
        end calculate_gap;

            procedure calculate_align is
            next_x : Natural := LOT_Parent.x; -- variable to calculate the next x-coord of siblings when calculating left alignment
            next_y : Natural := LOT_Parent.y; -- variable to calculate the next y-coord of siblings when calculating left alignment
            begin
            align_w := LOT_Parent.child_flex.align;
                case align_w is
                    when stretch =>
                        for i in Layout_Object_Tree.Iterate_Children (LOT, C)
                        loop
                            case LOT_Parent.child_flex.dir is
                                when left_right | right_left =>
                                    LOT (i).h := LOT_Parent_Height;

                                when bottom_top | top_bottom =>
                                    LOT (i).w := LOT_Parent_Width;

                                when others =>
                                    null;
                            end case;
                        end loop;

                    when center =>
                        for i in Layout_Object_Tree.Iterate_Children (LOT, C)
                        loop
                            case LOT_Parent.child_flex.dir is
                                when left_right | right_left =>
                                    LOT (i).y := (Lot_Parent_Height - LOT (i).h) / 2;

                                when bottom_top | top_bottom =>
                                    LOT (i).x := Lot_Parent_Width + LOT (i).w;

                                when others =>
                                    null;
                            end case;
                        end loop;

                    when bottom =>
                        for i in Layout_Object_Tree.Iterate_Children (LOT, C)
                        loop
                            LOT (i).y := LOT_Parent_Height - LOT_Offset_Y;
                        end loop;

                    --  when top =>
                    --      for i in Layout_Object_Tree.Iterate_Children (LOT, C)
                    --          loop
                    --              LOT (i).y := LOT_Parent_Height + Lot (i).h;
                    --          end loop;

                    when left =>
                        for i in Layout_Object_Tree.Iterate_Children (LOT, C)
                        loop
                            case LOT_Parent.child_flex.dir is
                                when left_right | right_left =>
                                    LOT (i).x := next_x;
                                    next_x := next_x + LOT (i).w;

                                when top_bottom | bottom_top =>
                                    LOT (i).x := LOT_Parent.x;
                                    LOT (i).y := next_y;
                                    next_y := next_y + LOT (i).h;

                                when others =>
                                    null;
                            end case;
                        end loop;

                    when right =>
                        for i in Layout_Object_Tree.Iterate_Children (LOT, C) 
                        loop
                            case LOT_Parent.child_flex.dir is
                                when left_right | right_left =>
                                    LOT (i).x := next_x;
                                    next_x := next_x + LOT (i).w;

                                when top_bottom | bottom_top =>
                                    LOT (i).x := LOT_Parent.x + LOT_Parent.w - LOT (i).w;
                                    LOT (i).y := next_y;
                                    next_y := next_y + LOT (i).h;
                                
                                when others =>
                                    null;
                            end case;
                        end loop;
                    when others =>
                        null; -- Currently not handling other alignment types
                end case;
            end calculate_align;

        begin
            if cc > 0 then
                begin
                    calculate_portions; -- Procedure call calculates data necessary to calculate (x,y) coordinates of widgets to be drawn.
                    calculate_children_coordinates; -- Procedure call traverses children of current widget to calculate their (x,y) coordinates.
                    if cc > 1 then
                        calculate_buoy; -- Procedure call recalculates (x,y) coordinates of child widgets to apply buoyancy format
                        calculate_gap; -- Procedure call recalculates (x,y) coordinates of child widgets to apply gap size
                        calculate_align;
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

    procedure handle_click_event (x_Input : Natural; y_Input : Natural) is

        procedure click_event (c : Layout_Object_Tree.Cursor) is
        begin
            if Layout_Object_Tree.Element (c).Is_In_Bound (x_Input, y_Input)
            then
                Layout_Object_Tree.Element (c).Click;
            end if;
        end click_event;

    begin
        Layout_Object_Tree.Iterate (LOT, click_event'Access);
    end handle_click_event;

    procedure handle_release_event is
        procedure release_event (c : Layout_Object_Tree.Cursor) is
        begin
            if Layout_Object_Tree.Element (c).Is_Clickable then
                Widget.Button.Any_Acc (Layout_Object_Tree.Element (c))
                   .release_click;
            end if;
        end release_event;
    begin
        Layout_Object_Tree.Iterate (LOT, release_event'Access);
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
