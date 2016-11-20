/******************************************/
/*                                        */
/*         PACKAGE GAME - PARTIE C        */
/*                                        */
/*          Gestion des erreurs           */
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

/* Lance une erreur SDL (s) propre au type (1 : SDL, 2 : SDL_Image, 3 : SDL_TTF, 4 : SDL_Mixer) */
void set_error(char *s, int type);

/* Récupère une erreur en fonction du type (cf. au dessus) */
char* get_error(int type);


/*****************************/
/* CORPS (cf. doc SDL_Mixer) */
/*****************************/

/* SET_ERROR */

void set_error(char *s, int type)
{
    /* On applique le SetError correspondant au type */
    if (type == 1)
	SDL_SetError(s);
    else if (type == 2)
	IMG_SetError(s);
    else if (type == 3)
	TTF_SetError(s);
    else if (type == 4)
	Mix_SetError(s);
}

/* GET_ERROR */

char* get_error(int type)
{
    /* On récupère l'erreur qu'il faut */
    if (type == 1)
	return SDL_GetError();
    else if (type == 2)
	return IMG_GetError();
    else if (type == 3)
	return TTF_GetError();
    else if (type == 4)
	return Mix_GetError();
}
