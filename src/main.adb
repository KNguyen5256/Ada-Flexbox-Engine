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

    header  : w.Any_Acc := w.Create (id         => "header",
                                      parent     => dui.main_widget,
                                      self_flex  => (expand_w => (behavior => max),
                                                     expand_h => (pixel, 50),
                                                     others   => <>),
                                      child_flex => (dir    => left_right,
                                                     others => <>),
                                      bgd        => get_color);
    file_button : w.Any_Acc := wb.Create (id         => "file_button",
                                      parent     => header,
                                      text       => "File",
                                      self_flex  => (expand_w => (pixel, 150),
                                                     expand_h => (pixel, 50),
                                                     others   => <>),
                                      bgd        => g.blue_4);
    edit_button : w.Any_Acc := wb.Create (id         => "edit_button",
                                      parent     => header,
                                      text       => "Edit",
                                      self_flex  => (expand_w => (pixel, 150),
                                                     expand_h => (pixel, 50),
                                                     others   => <>),
                                      bgd        => g.blue_5);
    gen_button : w.Any_Acc := wb.Create (id         => "gen_button",
                                      parent     => header,
                                      text       => "Generate",
                                      self_flex  => (expand_w => (pixel, 150),
                                                     expand_h => (pixel, 50),
                                                     others   => <>),
                                      bgd        => g.blue_6);

    images : w.Any_Acc := w.Create(id         => "images",
                                   parent     => dui.main_widget,
                                   self_flex  => (expand_w => (behavior => max),
                                                  expand_h => (behavior => max),
                                                  others   => <>),
                                   child_flex => (dir      => left_right,
                                                  others   => <>),
                                   bgd        => get_color);
    
    process : w.Any_Acc := wt.Create (id         => "process", -- causes runtime error when trying to shrink window
                                      parent     => dui.main_widget,
                                      text       => "Console",
                                      self_flex  => (expand_w => (behavior => max),
                                                     expand_h => (pixel, 50),
                                                     others => <>),
                                      child_flex => (dir      => left_right,
                                                     others   => <>),
                                      bgd        => g.black);
    
    tools   : w.Any_Acc := w.Create (id         => "tools", -- runtime error when rectangle is attempted to be drawn past (0,0) in bottom_top
                                      parent     => images,
                                      self_flex  => (expand_w => (pixel, 200),
                                                     expand_h => (behavior => max),
                                                     others   => <>),
                                      child_flex => (dir    => right_left,
                                                     buoy   => space_nothing,
                                                     others => <>),
                                      bgd        => get_color);
    add_contain : w.Any_Acc := wb.Create (id         => "add_contain",
                                      parent     => tools,
                                      text       => "Add Container",
                                      self_flex  => (expand_w => (behavior => max),
                                                     expand_h => (pixel, 100),
                                                     others   => <>),
                                                     max_height => 10,
                                      bgd        => g.red_1);
    add_flex : w.Any_Acc := wb.Create (id         => "add_flex",
                                      parent     => tools,
                                      text       => "Add Flex Item",
                                      self_flex  => (expand_w => (behavior => max),
                                                     expand_h => (pixel, 100),
                                                     others   => <>),
                                                     max_height => 15,
                                      bgd        => g.red_2);
    add_media : w.Any_Acc := wb.Create (id         => "add_media",
                                      parent     => tools,
                                      text       => "Add Media",
                                      self_flex  => (expand_w => (behavior => max),
                                                     expand_h => (pixel, 100),
                                                     others   => <>),
                                                     max_height => 20,
                                      bgd        => g.red_3);
    
    gui_area : w.Any_Acc := w.Create (id            => "gui_area",
                                       parent        => images,
                                       self_flex     => (expand_h => (behavior => max),
                                                         others   => <>),
                                       child_flex    => (--buoy => space_even,
                                                         --align => stretch,
                                                         dir => bottom_top,
                                                         --gap_r => (pixel, 10),
                                                         --gap_c => (percent, 0.02),
                                                         others   => <>),
                                       bgd           =>  g.white);
    orig_img : w.Any_Acc := wi.Create (id           => "orig_img",
                                       parent       => gui_area,
                                       abs_filename => "data/Ada_Lovelace_photo.qoi",
                                       self_flex    => (expand_w => (pixel, 200),
                                                        expand_h => (percent, 0.2),
                                                        others   => <>),
                                       bgd          => get_color);
    orig_img2 : w.Any_Acc := wi.Create (id           => "orig_img2",
                                       parent       => gui_area,
                                       abs_filename => "data/Ada_Lovelace_photo.qoi",
                                       self_flex    => (expand_w => (pixel, 200),
                                                        expand_h => (percent, 0.2),
                                                        others   => <>),
                                       bgd          => get_color);
    orig_img3 : w.Any_Acc := wi.Create (id           => "orig_img3",
                                       parent       => gui_area,
                                       abs_filename => "data/Ada_Lovelace_photo.qoi",
                                       self_flex    => (expand_w => (pixel, 200),
                                                        expand_h => (percent, 0.2),
                                                        others   => <>),
                                       bgd          => get_color);
    orig_img4 : w.Any_Acc := wi.Create (id           => "orig_img4",
                                       parent       => gui_area,
                                       abs_filename => "data/Ada_Lovelace_photo.qoi",
                                       self_flex    => (expand_w => (pixel, 200),
                                                        expand_h => (percent, 0.2),
                                                        others   => <>),
                                       bgd          => get_color);
    orig_img5 : w.Any_Acc := wi.Create (id           => "orig_img5",
                                       parent       => gui_area,
                                       abs_filename => "data/Ada_Lovelace_photo.qoi",
                                       self_flex    => (expand_w => (pixel, 200),
                                                        expand_h => (percent, 0.2),
                                                        others   => <>),
                                       bgd          => get_color);

    img_sep  : w.Any_Acc := w.Create (id         => "img_sep",
                                      parent     => images,
                                      self_flex  => (expand_w => (pixel, 10),
                                                     expand_h => (behavior => max),
                                                     others   => <>),
                                      bgd        => get_color);

    widget_list : w.Any_Acc := wt.Create (id            => "widget_list",
                                       parent        => images,
                                       text          => "Widget List",
                                       magnification => 1,
                                       self_flex     => (expand_w => (pixel, 300),
                                                         expand_h => (behavior => max),
                                                         others   => <>),
                                       child_flex    => (dir    => bottom_top,
                                                         others => <>),
                                       bgd           => g.light_grey);
    
    render_ui : x11_window.Update_Image_Buffer := dui.render'Access;
begin
    x11_window.open_window (1_848, 1_016, 60, render_ui);
end Main;
