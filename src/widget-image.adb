with Ada.Text_IO; use Ada.Text_IO;
with dui;

with namespaces; use namespaces;

with Ada.Real_Time; use Ada.Real_Time;

package body Widget.Image is

    
    function Create (id            : string;
                     parent        : Widget.Any_Acc;
                     abs_filename  : string;
                     self_flex     : flex_t  := default_flex;
                     child_flex    : flex_t  := default_flex;
                     bgd           : graphic.color) return Widget.Any_Acc is

        img  : aliased g.image := g.Load_QOI (abs_filename);
        This : Widget.Any_Acc;
    begin
        --aio.Put_Line ("create");

        This := new Instance' (af.Controlled with
                              id            => +id,
                              image         => img'Unchecked_Access,

                              abs_filename  => +abs_filename,
                              self_flex     => self_flex,
                              child_flex    => child_flex,
                              bgd           => bgd,
                              others        => <>);
        dui.add_to_LOT (This, Parent);
        return This;
    end;


    overriding procedure finalize (This : in out Instance) is
    begin
        null;
        -- if this.image /= null then
        --     g.free (this.image);
        -- end if;
    end;

    overriding procedure Event (This : in out Instance; Evt : Event_Kind) is
    begin
        null;
    end;

    overriding procedure Draw (This : in out Instance; img : in out g.image) is
        use dui;
        --Start_Time            : Time;
        --Elapsed_Time          : Time_Span;
    begin
        --Start_Time := Clock;
        dui.draw_image (img, this.image, this.x, this.y, this.w, this.h);
        -- Elapsed_Time := Clock - Start_Time;
        -- aio.Put_Line ("Elapsed time (draw image): "
        --    & Duration'Image (To_Duration (Elapsed_Time))
        --    & " seconds");
    end;

    overriding procedure Who_I_Am (This : in out Instance) is 
    begin
    Put_Line("I am an image widget");
    end;

end Widget.Image;