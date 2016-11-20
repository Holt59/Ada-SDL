/******************************************/
/*                                        */
/*         PACKAGE GAME - PARTIE C        */
/*                                        */
/*        Gestion des outils de base      */
/*                                        */
/* Créateur : CAPELLE Mikaël              */
/* Adresse  : capelle.mikael@gmail.com    */
/*                                        */
/* Dernière Modification : 06 / 06 / 2011 */
/*                                        */
/******************************************/


#include <SDL.h>
#include <SDL_image.h>
#include <SDL_ttf.h>
#include <SDL_mixer.h>


/*****************/
/* SPECIFICATION */
/*****************/

/* Initialise le(s) module(s) souhaité(s) */
int init(int timer, int video, int audio, int cdrom, int joystick, int ttf, int everything, int frequency);

/* Ferme tous les modules de la librairie */ 
int quit(void);

/* Retourne la largeur de la surface "S" */
int get_width(SDL_Surface *S);

/* Retourne la hauteur de la surface "S" */
int get_height(SDL_Surface *S);

/* Retourne la profondeur (Bits par pixel) de la surface "S" */
int get_depth (SDL_Surface *S);

/* Change le titre de la fenêtre */
void change_title(const char *S);

/* Change l'icone du programme */
void change_icon_string(const char *icon_path);

void change_icon_surface(SDL_Surface *S);


/*********/
/* CORPS */
/*********/

/* INIT */

int init(int timer, int video, int audio, int cdrom, int joystick, int ttf, int everything, int frequency)
{
    int flag = 0;
        
    /* Si on veut tout initialiser */
    if (everything)
    {
	if (SDL_Init(SDL_INIT_EVERYTHING) == -1)
	    return -1;
	
	if (TTF_Init() == -1)
	    return -2;

	if (Mix_OpenAudio(frequency, MIX_DEFAULT_FORMAT, 2, 1024) == -1)
	    return -3;

	return 0;
    }        

    /* Sinon, on modifie le flag en fonction de ce que l'on veut initialiser */
    if (timer)
	flag |= SDL_INIT_TIMER;
    if (video)
	flag |= SDL_INIT_VIDEO;
    if (cdrom)
	flag |= SDL_INIT_CDROM;
    if (joystick)
	flag |= SDL_INIT_JOYSTICK;

    /* Puis on initialise ce qu'il faut */
    if (audio)
	if (Mix_OpenAudio(frequency, MIX_DEFAULT_FORMAT, 2, 1024) == -1)
	    return -3;

    if (ttf)
	if (TTF_Init() == -1)
	    return -2;

    if (SDL_Init(flag) == -1)
	return -1;

    return 0;
}

/* QUIT */

int quit(void)
{
    /* On ferme tout */
    Mix_CloseAudio();
    TTF_Quit();
    SDL_Quit();
}

/* GET_WIDTH */

int get_width(SDL_Surface *S)
{
    return S->w;
}

/* GET_HEIGHT */

int get_height(SDL_Surface *S)
{
    return S->h;
}

/* GET_DEPTH */

int get_depth (SDL_Surface *S)
{
    return S->format->BitsPerPixel;
}

/* CHANGE_TITLE */

void change_title(const char *S)
{
    SDL_WM_SetCaption(S,NULL);
}

/* CHANGE_ICON_STRING */

void change_icon_string(const char *icon_path)
{
    SDL_Surface *tmp = NULL;
    if ((tmp = IMG_Load(icon_path)) != NULL)
	SDL_WM_SetIcon(tmp,NULL);
    SDL_FreeSurface(tmp);
}

/* CHANGE_ICONE_SURFACE */

void change_icon_surface(SDL_Surface *S)
{
    SDL_WM_SetIcon(S,NULL);
}
