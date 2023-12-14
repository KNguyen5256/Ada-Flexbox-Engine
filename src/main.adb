with Ada.Text_IO; use Ada.Text_IO;
with dui;         use dui;

with graphic; use graphic;
with importer;
with x11_window;
with Ada.Strings.Unbounded;

with Ada.Finalization; use Ada.Finalization;

with font;

with Widget; use Widget;
with Widget.Button;
with Widget.Text;
with Widget.Image;

procedure Main is

    package w renames Widget;
    package wt renames Widget.Text;
    package wi renames Widget.Image;
    package wb renames Widget.Button;
    package g renames graphic;

    color_counter : Float       := 1.0;
    mult          : g.color_val := 0.01;

    function get_val (counter : Float) return g.color_val is
    begin
        return 0.3 + g.color_val (counter * Float (mult));
    end get_val;

    function get_color return g.color is
        c : g.color;
    begin
        color_counter := color_counter + 1.0;
        c             :=
           (get_val (color_counter), get_val (color_counter),
            get_val (color_counter), 0.0);
        return c;
    end get_color;

    header      : w.Any_Acc :=
       w.Create
          (id => "Header", parent => dui.main_widget,
                                  --text       => "Menu Bar",
                                  self_flex =>
              (expand_w => (behavior => max), expand_h => (behavior => max),
               others   => <>),
           child_flex => (dir => left_right,others => <>), bgd => get_color);
    file_button : w.Any_Acc :=
       wb.Create
          (id => "Element 1", parent => header,
                                        --text       => "File",
                                        self_flex =>
              (expand_w => (pixel, 50), expand_h => (pixel, 50),
               others   => <>),
           bgd                                    => g.blue_4);
    edit_button : w.Any_Acc :=
       wb.Create
          (id => "Element 2", parent => header,
                                        --text       => "Edit",
                                        self_flex =>
              (expand_w => (pixel, 100), expand_h => (pixel, 100),
               others   => <>),
           bgd                                    => g.blue_5);
    gen_button  : w.Any_Acc :=
       wb.Create
          (id => "Element 3", parent => header,
                                       --text       => "Generate",
                                       self_flex =>
              (expand_w => (pixel, 150), expand_h => (pixel, 150),
               others   => <>),
           bgd                                   => g.blue_6);

    render_ui : x11_window.Update_Image_Buffer := dui.render'Access;
begin
    x11_window.open_window (1_848, 1_016, 60, render_ui);
end Main;
