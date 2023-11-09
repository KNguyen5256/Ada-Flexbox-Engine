with ada.Text_IO; use ada.Text_IO;
with Interfaces.C.Strings;
with Interfaces;
with system;
with dui; use dui;

with Ada.Real_Time; use Ada.Real_Time;

with X;
with X.Xlib;
with X.Strings;
with Widget; use Widget;
with widget.Button;

with namespaces; use namespaces;

package body x11_window is

    package ics renames Interfaces.C.Strings;
    package ic  renames interfaces.C;

    use Interfaces;

    function wrap_alloc_xshm_image(display      : X.Xlib.XDisplay_access;
                                   visual       : X.Xlib.Visual_access;
                                   width        : ic.int;
                                   height       : ic.int;
                                   depth        : ic.int) return X.Xlib.XImage_access with
        Import        => True,
        Convention    => c,
        External_Name => "wrap_alloc_xshm_image";

    function x_shm_put_image (display    : X.Xlib.XDisplay_access;
                              d          : X.Drawable;
                              gc         : X.Xlib.GC;
                              image      : X.Xlib.XImage_access;
                              src_x      : ic.int;
                              src_y      : ic.int;
                              dst_x      : ic.int;
                              dst_y      : ic.int;
                              src_w      : ic.unsigned;
                              src_h      : ic.unsigned;
                              send_event : X.Xlib.bool) return X.Xlib.bool with
        Import        => True,
        Convention    => c,
        External_Name => "XShmPutImage";

    procedure ui_to_x_shm (ui_img        : graphic.image; 
                           x_shm_image   : X.Strings.charp;
                           window_width  : natural;
                           window_height : natural) is
        pragma Suppress (All_checks);
        buf_w : natural := ui_img'length(1);
        buf_h : natural := ui_img'length(2);
        rgb_img : g.image (1 .. buf_h, 1 .. buf_w) with Address => x_shm_image.all'address;        
    begin
        for j in 1 .. window_height loop
            for i in 1 .. window_width loop
                rgb_img (j, i) := ui_img (i,j);
            end loop;
        end loop;
    end;

    procedure open_window (window_width  : Natural; 
                           window_height : Natural; 
                           fps           : Integer;
                           render_ui     : Update_Image_Buffer)
    is
        use type Interfaces.C.int;
        use type Interfaces.C.unsigned;
        use type X.Xlib.XDisplay_access;

        Cant_Open_Display_Error : exception;

        Display          : X.Xlib.XDisplay_access;
        Screen_Num       : ic.int;
        Win, Root_Win    : X.Window;
        Graphics_Context : X.Xlib.GC;
        Report           : aliased X.Xlib.XEvent;
        w                : ic.unsigned := ic.unsigned(window_width);
        h                : ic.unsigned := ic.unsigned(window_height);
        quit             : boolean := False;

        ui_img           : graphic.image_access; 
        img              : X.Xlib.XImage_access;
        res              : X.Xlib.Status; 
        wa               : aliased X.Xlib.XWindowAttributes;
    begin
        Display := X.Xlib.XOpenDisplay (null);
        

        if Display = null then
            raise Cant_Open_Display_Error;
        end if;

        Screen_num := X.Xlib.DefaultScreen (Display);

        Root_Win := X.Xlib.RootWindow (Display, Screen_Num);

        Win := X.Xlib.XCreateSimpleWindow (Display, Root_Win, 0, 0, w, h, 0, X.Xlib.BlackPixel (Display, Screen_Num),
                                                                             X.Xlib.BlackPixel (Display, Screen_Num));

        X.Xlib.XMapWindow (display, Win);

        X.Xlib.XSelectInput (Display, Win, X.StructureNotifyMask);

        loop
            X.Xlib.XNextEvent (Display, Report'Access);
            exit when Report.Event_Type = X.MapNotify;
        end loop;

        Graphics_Context := X.Xlib.XDefaultGC (Display, Screen_Num);

        ui_img := new graphic.image(1 .. window_width, 1 .. window_height);

        res := X.Xlib.XGetWindowAttributes (Display, Win, Wa'Unchecked_Access);
        img := wrap_alloc_xshm_image (display, wa.visual, ic.int (w), ic.int (h), 24);

        X.Xlib.XSelectInput (Display, Win, ic.Long (ic.Unsigned (X.ButtonPressMask) or X.ButtonReleaseMask));

        loop

            while X.Xlib.XPending (display) > 0 loop
                X.Xlib.XNextEvent (Display, Report'Access);
                if(Report.Event_Type = X.ButtonPress) then
                    dui.handle_click_event(Natural(Report.xbutton.xx), Natural(Report.xbutton.y));
                end if;
            end loop;
 
            res := X.Xlib.XGetWindowAttributes (Display, Win, Wa'Unchecked_Access);

            render_ui (ui_img.all, integer (wa.width), integer (wa.height));

            ui_to_x_shm (ui_img.all, img.data, integer (wa.width), integer (wa.height));

            res := ic.int (x_shm_put_image (display, X.Drawable (win), Graphics_Context, img, 0, 0, 0, 0, w, h, 0));

            --exit when Report.Event_Type = X.ButtonRelease;
            exit when quit;
        end loop;

        X.Xlib.XDestroyWindow (Display, Win);
        X.Xlib.XCloseDisplay (Display);
    end open_window;

end x11_window;