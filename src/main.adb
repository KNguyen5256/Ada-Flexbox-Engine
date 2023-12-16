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
                                     child_flex => (dir      => left_right,
                                                    others   => <>),
                                     bgd        => get_color);
    h_button1 : w.Any_Acc := wb.Create (id         => "h_button1",
                                        parent     => header,
                                        text       => "File",
                                        self_flex  => (expand_w => (pixel, 150),
                                                       expand_h => (pixel, 50),
                                                       others   => <>),
                                        bgd        => g.blue_4);
    h_button2 : w.Any_Acc := wb.Create (id         => "h_button2",
                                        parent     => header,
                                        text       => "Edit",
                                        self_flex  => (expand_w => (pixel, 150),
                                                       expand_h => (pixel, 50),
                                                       others   => <>),
                                        bgd        => g.blue_5);
    h_button3 : w.Any_Acc := wb.Create (id         => "h_button3",
                                        parent     => header,
                                        text       => "Generate",
                                        self_flex  => (expand_w => (pixel, 150),
                                                       expand_h => (pixel, 50),
                                                       others   => <>),
                                        bgd        => g.blue_6);

    widget_area : w.Any_Acc := w.Create(id         => "widget_area",
                                        parent     => dui.main_widget,
                                        self_flex  => (expand_w => (behavior => max),
                                                       expand_h => (behavior => max),
                                                       others   => <>),
                                        child_flex => (dir      => left_right,
                                                       others   => <>),
                                        bgd        => get_color);
    
    tools   : w.Any_Acc := w.Create (id         => "tools", -- runtime error when rectangle is attempted to be drawn past (0,0) in bottom_top
                                     parent     => widget_area,
                                     self_flex  => (expand_w => (pixel, 200),
                                                    expand_h => (behavior => max),
                                                    others   => <>),
                                     child_flex => (dir      => top_bottom,
                                                    buoy     => space_nothing,
                                                    others   => <>),
                                     bgd        => get_color);
    t_button1 : w.Any_Acc := wb.Create (id         => "t_button1",
                                        parent     => tools,
                                        text       => "Add Container",
                                        self_flex  => (expand_w => (behavior => max),
                                                       expand_h => (pixel, 100),
                                                       others   => <>),
                                        max_height => 50,
                                        bgd        => g.red_1);
    t_button2 : w.Any_Acc := wb.Create (id         => "t_button2",
                                        parent     => tools,
                                        text       => "Add Flex Item",
                                        self_flex  => (expand_w => (behavior => max),
                                                       expand_h => (pixel, 100),
                                                       others   => <>),
                                        max_height => 50,
                                        bgd        => g.red_2);
    t_button3 : w.Any_Acc := wb.Create (id         => "t_button3",
                                        parent     => tools,
                                        text       => "Add Media",
                                        self_flex  => (expand_w => (behavior => max),
                                                       expand_h => (pixel, 100),
                                                       others   => <>),
                                        max_height => 50,
                                        bgd        => g.red_3);
    
    test_area : w.Any_Acc := w.Create (id            => "test_area",
                                       parent        => widget_area,
                                       self_flex     => (expand_h => (behavior => max),
                                                         others   => <>),
                                       child_flex    => (dir      => left_right,
                                                         others   => <>),
                                       bgd           =>  g.white);
    orig_img1 : w.Any_Acc := wi.Create (id           => "orig_img1",
                                        parent       => test_area,
                                        abs_filename => "data/Ada_Lovelace_photo.qoi",
                                        self_flex    => (expand_w => (pixel, 200),
                                                         expand_h => (percent, 0.2),
                                                         others   => <>),
                                        bgd          => get_color);
    orig_img2 : w.Any_Acc := wi.Create (id           => "orig_img2",
                                        parent       => test_area,
                                        abs_filename => "data/Ada_Lovelace_photo.qoi",
                                        self_flex    => (expand_w => (pixel, 200),
                                                         expand_h => (percent, 0.2),
                                                         others   => <>),
                                        bgd          => get_color);
    orig_img3 : w.Any_Acc := wi.Create (id           => "orig_img3",
                                        parent       => test_area,
                                        abs_filename => "data/Ada_Lovelace_photo.qoi",
                                        self_flex    => (expand_w => (pixel, 200),
                                                         expand_h => (percent, 0.2),
                                                         others   => <>),
                                        bgd          => get_color);
    orig_img4 : w.Any_Acc := wi.Create (id           => "orig_img4",
                                        parent       => test_area,
                                        abs_filename => "data/Ada_Lovelace_photo.qoi",
                                        self_flex    => (expand_w => (pixel, 200),
                                                         expand_h => (percent, 0.2),
                                                         others   => <>),
                                        bgd          => get_color);
    orig_img5 : w.Any_Acc := wi.Create (id           => "orig_img5",
                                        parent       => test_area,
                                        abs_filename => "data/Ada_Lovelace_photo.qoi",
                                        self_flex    => (expand_w => (pixel, 200),
                                                         expand_h => (percent, 0.2),
                                                         others   => <>),
                                        bgd          => get_color);

    thin_widget  : w.Any_Acc := w.Create (id         => "thin_widget",
                                          parent     => widget_area,
                                          self_flex  => (expand_w => (pixel, 10),
                                                         expand_h => (behavior => max),
                                                         others   => <>),
                                          bgd        => get_color);

    widget_list : w.Any_Acc := wt.Create (id            => "widget_list",
                                          parent        => widget_area,
                                          text          => "Widget List",
                                          magnification => 1,
                                          self_flex     => (expand_w => (pixel, 300),
                                                            expand_h => (behavior => max),
                                                            others   => <>),
                                          child_flex    => (dir      => bottom_top,
                                                            others   => <>),
                                          bgd           => g.light_grey);
    
    footer : w.Any_Acc := wt.Create (id         => "footer", -- causes runtime error when trying to shrink window
                                     parent     => dui.main_widget,
                                     text       => "Console",
                                     self_flex  => (expand_w => (behavior => max),
                                                    expand_h => (pixel, 50),
                                                    others   => <>),
                                     child_flex => (dir      => left_right,
                                                    others   => <>),
                                     bgd        => g.black);
    
    render_ui : x11_window.Update_Image_Buffer := dui.render'Access;
begin
    x11_window.open_window (1_848, 1_016, 60, render_ui);
end Main;
