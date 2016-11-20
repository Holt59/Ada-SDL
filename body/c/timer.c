/******************************************/
/*                                        */
/*         PACKAGE GAME - PARTIE C        */
/*                                        */
/*            Gestion des timers          */
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

/* Retourne le temps d'utilisation du processeur depuis le lancement de la librairie */
Uint32 get_tick(void);

/* Met le programme en pause (0% de processeur) pendant ms millisecondes */
void wait(Uint32 ms);


/*********/
/* CORPS */
/*********/

/* GET_TICK */

Uint32 get_tick(void)
{
    return SDL_GetTicks();
}

/* WAIT */

void wait(Uint32 ms)
{
    SDL_Delay(ms);
}
