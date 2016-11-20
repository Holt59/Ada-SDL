--------------------------------------------
--                                        --
--       PACKAGE GAME - PARTIE ADA        --
--                                        --
--            GAME-GEVENT.ADS             --
--                                        --
--         Gestion des évènements         --
--                                        --
-- Créateur : CAPELLE Mikaël              --
-- Adresse  : capelle.mikael@gmail.com    --
--                                        --
-- Dernière modification : 14 / 06 / 2011 --
--                                        --
--------------------------------------------

package Game.GEvent is

   -- Voir plus bas
   type Event;

   -- Les différents types d'event possible
   type Event_Type is (ACTIVE, KEYDOWN, KEYUP, MOUSE_MOTION, MOUSE_BUTTON_DOWN,
                       MOUSE_BUTTON_UP, QUIT,NONE);

   -- Les différentes valeurs des touches du clavier (sur un clavier QWERTY !!)
   type Event_Key is (K_BACKSPACE,K_TAB,K_CLEAR,K_RETURN,K_PAUSE,K_ESCAPE,K_SPACE,
                      K_EXCLAIM,K_QUOTEDBL,K_HASH,K_DOLLAR,K_AMPERSAND,K_QUOTE,
                      K_LEFTPAREN,K_RIGHTPAREN,K_ASTERISK,K_PLUS,K_COMMA,K_MINUS,
                      K_PERIOD,K_SLASH,
                      -- TOUCHE NOMBRE (AU DESSUS DES LETTRES)
                      K_0,K_1,K_2,K_3,K_4,K_5,K_6,K_7,K_8,K_9,
                      K_COLON,K_SEMICOLON,K_LESS,K_EQUALS,K_GREATER,K_QUESTION,
                      K_AT,K_LEFTBRACKET,K_BACKSLASH,K_RIGHTBRACKET,K_CARET,
                      K_UNDERSCORE,K_BACKQUOTE,
                      -- TOUCHE LETTRES (CLAVIER QWERTY !!)
                      K_A,K_B,K_C,K_D,K_E,K_F,K_G,K_H,K_I,K_J,K_K,K_L,K_M,K_N,K_O,K_P,K_Q,K_R,K_S,K_T,K_U,K_V,K_W,K_X,K_Y,K_Z,
                      K_DELETE,
                      -- TOUCHE DU PAVE NUMERIQUE (K_KP)
                      K_KP0,K_KP1,K_KP2,K_KP3,K_KP4,K_KP5,K_KP6,K_KP7,K_KP8,K_KP9,
                      K_KP_PERIOD,K_KP_DIVIDE,K_KP_MULTIPLY,K_KP_MINUS,K_KP_PLUS,K_KP_ENTER,K_KP_EQUALS,
                      -- FLECHES DIRECTIONNELLES
                      K_UP,K_DOWN,K_RIGHT,K_LEFT,
                      K_INSERT,K_HOME,K_END,K_PAGEUP,K_PAGEDOWN,
                      -- TOUCHE DE FONCTION (F1..F15)
                      K_F1,K_F2,K_F3,K_F4,K_F5,K_F6,K_F7,K_F8,K_F9,K_F10,K_F11,K_F12,K_F13,K_F14,K_F15,
                      K_NUMLOCK,K_CAPSLOCK,K_SCROLLOCK,K_RSHIFT,
                      K_LSHIFT,K_RCTRL,K_LCTRL,K_RALT,K_LALT,K_RMETA,K_LMETA,
                      K_LSUPER,K_RSUPER,K_MODE,K_HELP,K_PRINT,K_SYSREQ,
                      K_BREAK,K_MENU,K_POWER,K_EURO);

   -- Les boutons de la souris
   type Event_Mouse_Key is (LEFT,MIDDLE,RIGHT,WHEELUP,WHEELDOWN);

   type Tab_Key   is array (Event_Key) of Boolean;
   type Tab_Mouse is array (Event_Mouse_Key) of Boolean;

   -- Type contenant toutes les informations à propos des events
   type Event is
      record
         Etype        : Event_Type := NONE;                 -- Le type de l'event
         Ekey         : Tab_Key    := (others => False);    -- Array (Event_Key range K_BACKSPACE..K_EURO) of Boolean;
         X,Y          : Integer    := -1;                   -- Les coordonnées de la souris, sinon (-1,-1)
         X_Rel, Y_Rel : Integer    := 0;                    -- Les coordonnées du mouvement de la souris
         EMouseButton : Tab_Mouse  := (others => False);    -- Array (Event_Mouse_Key range LEFT..RIGHT) of Boolean;
      end record;

   -- Retourne un nouvel event (ré)initialisé par (NONE,(others => False),-1,-1,0,0,(others => False))
   function New_Event return Event;

   -- Attends qu'un event arrive et le retourne, cette fonction bloque le
   -- programme tant qu'elle ne reçoit pas un évènement
   procedure Wait_Event (E : in out Event);

   -- Regarde si il y a des events en attente
   -- Si il y en a, E est mis à jour avec le premier event de la queue et
   -- En_Of_Queue est mis à False
   -- Sinon E n'est pas modifié et End_Of_Queue est mis à True
   procedure Poll_Event(E            : in out Event;
                        End_Of_Queue :    out Boolean);

   -- Enable_Key_Repeat active la répétition lors de la génération d'event
   -- au clavier : Lorsqu'il est activé, l'appuie sur une touche génère
   -- un premier event, puis après Wait milliseconde, elle génère un event
   -- toutes les Interval milliseconde tant qu'elle n'est pas relevé
   -- Mettre Wait à 0 desactive la répétition, pour plus de clareté, il
   -- est conseillé d'utiliser la fonction de désactivation
   procedure Enable_Key_Repeat(Wait     : in Positive := 500;
                               Interval : in Positive := 30);
   procedure Disable_Key_Repeat;

end Game.GEvent;
