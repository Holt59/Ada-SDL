Ada SDL Bindings
================

Ada bindings for the [Simple DirectMedia Layer (SDL)](https://www.libsdl.org/) library.

This repository contains simple bindings for the SDL library that can be used in Ada.

Sample program
==============

```ada
with Ada.Exceptions;
with Ada.Text_IO;
with Game;
with Game.Display;
with Game.GAudio;
with Game.GEvent; Game.Gtype;

procedure Main is

    WIDTH : constant Integer := 800;
    HEIGHT : constant Integer := 600;

begin

   Game.Init;

   Change_Title("Sample Program");
   Change_Icon("icon.jpg");

   Game.Display.Set_Video(WIDTH, HEIGHT);

   Quit;

exception
   when Except : Game.Init_Error =>
      Ada.Text_IO.Put(Ada.Exceptions.Exception_Information(Except));
   when Game.Video_Error =>
      Ada.Text_IO.Put_Line("Erreur while loading video!");

end Main;
```


Copyright and license
=====================

The MIT License (MIT)

Copyright (c) 2016, Mikaël Capelle.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

See [LICENSE](LICENSE).
