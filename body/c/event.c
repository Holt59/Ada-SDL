/******************************************/
/*                                        */
/*         PACKAGE GAME - PARTIE C        */
/*                                        */
/*         Gestion des évènements         */
/*                                        */
/* Créateur : CAPELLE Mikaël              */
/* Adresse  : capelle.mikael@gmail.com    */
/*                                        */
/* Dernière Modification : 06 / 06 / 2011 */
/*                                        */
/******************************************/


#include <SDL.h>


/*****************/
/* SPECIFICATION */
/*****************/

/* C_EVENT */

/* type pour stocker de façon "plus simple" que SDL_Event un évènement */
typedef struct
{
    int etype;        /* Type de l'event */ 
    int ekey;         /* Touche du clavier */
    int x,y;          /* Position de la souris (absolu) */
    int x_rel, y_rel; /* Position de la souris (relative) */
    int embutton;     /* Bouton de la souris */
} c_event;

/* Transforme une valeur pour correspondre avec la valeur ADA */
int switch_key(int key);

/* Attends qu'un event se produise puis le renvoie (met le programme en pause en attendant */
c_event wait_event(void);

/* Test si un event est disponible, si oui met "ok" à 1 et stocke l'event dans "S" */
void poll_event(c_event *S, int *ok);

/* Transforme un SDL_Event en c_event */
c_event switch_event(SDL_Event event);

/* Retourne les éléments d'un SDL_Event en fonction du type de l'évènement (touche clavier, clic souris, mouvement souris) */
c_event key_event(SDL_Event e);
c_event mouse_event_1(SDL_Event event);
c_event mouse_event_2(SDL_Event event);

/* Active la répétition des touches */
void enable_key_repeat(int delay, int interval);

/*  Desactive la répétition des touches */
void disable_key_repeat(void);


/*********/
/* CORPS */
/*********/

/* SWITCH_KEY */

int switch_key(int key)
{
    switch (key)
    {
    case SDLK_BACKSPACE:return 0;
    case SDLK_TAB:return 1;
    case SDLK_CLEAR:return 2;
    case SDLK_RETURN:return 3;
    case SDLK_PAUSE:return 4;
    case SDLK_ESCAPE:return 5;
    case SDLK_SPACE:return 6;
    case SDLK_EXCLAIM:return 7;
    case SDLK_QUOTEDBL:return 8;
    case SDLK_HASH:return 9;
    case SDLK_DOLLAR:return 10;
    case SDLK_AMPERSAND:return 11;
    case SDLK_QUOTE:return 12;
    case SDLK_LEFTPAREN:return 13;
    case SDLK_RIGHTPAREN:return 14;
    case SDLK_ASTERISK:return 15;
    case SDLK_PLUS:return 16;
    case SDLK_COMMA:return 17;
    case SDLK_MINUS:return 18;
    case SDLK_PERIOD:return 19;
    case SDLK_SLASH:return 20;
    case SDLK_0:return 21;
    case SDLK_1:return 22;
    case SDLK_2:return 23;
    case SDLK_3:return 24;
    case SDLK_4:return 25;
    case SDLK_5:return 26;
    case SDLK_6:return 27;
    case SDLK_7:return 28;
    case SDLK_8:return 29;
    case SDLK_9:return 30;
    case SDLK_COLON:return 31;
    case SDLK_SEMICOLON:return 32;
    case SDLK_LESS:return 33;
    case SDLK_EQUALS:return 34;
    case SDLK_GREATER:return 35;
    case SDLK_QUESTION:return 36;
    case SDLK_AT:return 37;
    case SDLK_LEFTBRACKET:return 38;
    case SDLK_BACKSLASH:return 39;
    case SDLK_RIGHTBRACKET:return 40;
    case SDLK_CARET:return 41;
    case SDLK_UNDERSCORE:return 42;
    case SDLK_BACKQUOTE:return 43;
    case SDLK_a:return 44;
    case SDLK_b:return 45;
    case SDLK_c:return 46;
    case SDLK_d:return 47;
    case SDLK_e:return 48;
    case SDLK_f:return 49;
    case SDLK_g:return 50;
    case SDLK_h:return 51;
    case SDLK_i:return 52;
    case SDLK_j:return 53;
    case SDLK_k:return 54;
    case SDLK_l:return 55;
    case SDLK_m:return 56;
    case SDLK_n:return 57;
    case SDLK_o:return 58;
    case SDLK_p:return 59;
    case SDLK_q:return 60;
    case SDLK_r:return 61;
    case SDLK_s:return 62;
    case SDLK_t:return 63;
    case SDLK_u:return 64;
    case SDLK_v:return 65;
    case SDLK_w:return 66;
    case SDLK_x:return 67;
    case SDLK_y:return 68;
    case SDLK_z:return 69;
    case SDLK_DELETE:return 70;
    case SDLK_KP0:return 71;
    case SDLK_KP1:return 72;
    case SDLK_KP2:return 73;
    case SDLK_KP3:return 74;
    case SDLK_KP4:return 75;
    case SDLK_KP5:return 76;
    case SDLK_KP6:return 77;
    case SDLK_KP7:return 78;
    case SDLK_KP8:return 79;
    case SDLK_KP9:return 80;
    case SDLK_KP_PERIOD:return 81;
    case SDLK_KP_DIVIDE:return 82;
    case SDLK_KP_MULTIPLY:return 83;
    case SDLK_KP_MINUS:return 84;
    case SDLK_KP_PLUS:return 85;
    case SDLK_KP_ENTER:return 86;
    case SDLK_KP_EQUALS:return 87;
    case SDLK_UP:return 88;
    case SDLK_DOWN:return 89;
    case SDLK_RIGHT:return 90;
    case SDLK_LEFT:return 91;
    case SDLK_INSERT:return 92;
    case SDLK_HOME:return 93;
    case SDLK_END:return 94;
    case SDLK_PAGEUP:return 95;
    case SDLK_PAGEDOWN:return 96;
    case SDLK_F1:return 97;
    case SDLK_F2:return 98;
    case SDLK_F3:return 99;
    case SDLK_F4:return 100;
    case SDLK_F5:return 101;
    case SDLK_F6:return 102;
    case SDLK_F7:return 103;
    case SDLK_F8:return 104;
    case SDLK_F9:return 105;
    case SDLK_F10:return 106;
    case SDLK_F11:return 107;
    case SDLK_F12:return 108;
    case SDLK_F13:return 109;
    case SDLK_F14:return 110;
    case SDLK_F15:return 111;
    case SDLK_NUMLOCK:return 112;
    case SDLK_CAPSLOCK:return 113;
    case SDLK_SCROLLOCK:return 114;
    case SDLK_RSHIFT:return 115;
    case SDLK_LSHIFT:return 116;
    case SDLK_RCTRL:return 117;
    case SDLK_LCTRL:return 118;
    case SDLK_RALT:return 119;
    case SDLK_LALT:return 120;
    case SDLK_RMETA:return 121;
    case SDLK_LMETA:return 122;
    case SDLK_LSUPER:return 123;
    case SDLK_RSUPER:return 124;
    case SDLK_MODE:return 125;
    case SDLK_HELP:return 126;
    case SDLK_PRINT:return 127;
    case SDLK_SYSREQ:return 128;
    case SDLK_BREAK:return 129;
    case SDLK_MENU:return 130;
    case SDLK_POWER:return 131;
    case SDLK_EURO:return 132;
    default:return -1;
    }
}

/* WAIT_EVENT */

c_event wait_event(void)
{
    SDL_Event event;
    if (SDL_WaitEvent(&event))
	return switch_event(event);
}

/* POLL_EVENT */

void poll_event(c_event *S, int *ok)
{
    SDL_Event event;
    if (SDL_PollEvent(&event))
    {
	*S = switch_event(event);
	*ok = 1;
    }
    else
	*ok = 0;
}

/* SWITCH_EVENT */

c_event switch_event(SDL_Event event)
{
    /* On initialise tout à 0 */
    c_event res = {0,0,0,0,0,0,0};

    /* En fonction du type de l'event, on change les valeurs qu'il faut */
    switch (event.type)
    {
    case SDL_QUIT:
	res.etype = 6;
	res.ekey = res.x = res.y = res.x_rel = res.y_rel = -1;
	res .embutton = -1;
	break;
    case SDL_KEYDOWN:
	res = key_event(event);
	break;
    case SDL_KEYUP:
	res = key_event(event);
	break;
    case SDL_MOUSEBUTTONUP:
    case SDL_MOUSEBUTTONDOWN:
	res = mouse_event_1(event);
	break;
    case SDL_MOUSEMOTION:
	res = mouse_event_2(event);
	break;
    default:
	res.etype = 0;
	break;
    }
    return res;
}

/* KEY_EVENT */

c_event key_event(SDL_Event e)
{
    c_event res;
    /* Si c'est un appuie ou un relachement */
    if (e.type == SDL_KEYDOWN)
	res.etype = 1;
    else
	res.etype = 2;
    /* On récupère la touche */
    res.ekey = switch_key(e.key.keysym.sym);
    /* On met les autres valeurs par défault */
    res.x = res.y = -1;
    res.x_rel = res.y_rel = -1;
    res.embutton = -1;
    return res;
}

/* MOUSE_EVENT_1 */

c_event mouse_event_1(SDL_Event event)
{
    c_event res;
    /* Si c'est un relachement ou un appuie */
    if (event.type == SDL_MOUSEBUTTONUP )
	res.etype = 5;
    else
	res.etype = 4;
    /* Si on a un clic droit / gauche / molette on récup, sinon on ne peut gérer le clic (=> 0) */
    res.embutton = (event.button.button <= SDL_BUTTON_WHEELDOWN) ? event.button.button - 1 : 0;
    /* On récup les coordonnées du clic */
    res.x = event.button.x;
    res.y = event.button.y;
    /* On met les autres valeurs par défault */
    res.ekey = res.x_rel = res.y_rel = -1;
    return res;
}

/* MOUSE_EVENT_2 */

c_event mouse_event_2(SDL_Event event)
{
    c_event res;
    res.etype = 3;
    res.embutton = -1;
    res.x = event.motion.x;
    res.y = event.motion.y;
    res.ekey = -1;
    res.x_rel = event.motion.xrel;
    res.y_rel = event.motion.yrel;
    return res;
}

/* ENABLE_KEY_REPET */

void enable_key_repeat(int delay, int interval)
{
    SDL_EnableKeyRepeat(delay,interval);
}

/* DISABLE_KEY_REPEAT */

void disable_key_repeat(void)
{
    SDL_EnableKeyRepeat(0,0);
}
