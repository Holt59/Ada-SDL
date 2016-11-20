/******************************************/
/*                                        */
/*         PACKAGE GAME - PARTIE C        */
/*                                        */
/*    Gestion de l'affichage du texte     */
/*                                        */
/* Créateur : CAPELLE Mikaël              */
/* Adresse  : capelle.mikael@gmail.com    */
/*                                        */
/* Dernière Modification : 06 / 06 / 2011 */
/*                                        */
/******************************************/


#include <SDL.h>
#include <SDL_ttf.h>
#include <string.h>


/*****************/
/* SPECIFICATION */
/*****************/

enum {SOLID=0,SHADED=1,BLENDED=2};
enum {LATIN=0,UTF8=1}; //,UNICODE=2};

enum {NORMAL,BOLD,ITALIC,UNDERLINE}; 

/* Ouverture de la police */
TTF_Font* open_font(const char *S, int taille);


/* Fermeture de la police */
void close_font(TTF_Font *f);

/* Creation d'une surface avec un texte à partir d'une police font */
SDL_Surface* create_text_font(const char *texte, TTF_Font *font, int type, int encode, int normal, int bold, int italic, int underline, SDL_Color *coul, SDL_Color *fond);


/*********/
/* CORPS */
/*********/

/* OPEN_FONT */

TTF_Font* open_font(const char *S, int taille)
{
    return TTF_OpenFont(S,taille);
}

/* CLOSE_FONT */

void close_font(TTF_Font *f)
{
    TTF_CloseFont(f);
}

/* CREATE_TEXT_FONT */

SDL_Surface* create_text_font(const char *texte, TTF_Font *font, int type, int encode, int normal, int bold, int italic, int underline, SDL_Color *coul, SDL_Color *fond)
{
    int flag = 0;
    SDL_Surface *res = NULL;

    /* Modification du flag en fonction des paramètres */
    if (normal)
    {
	flag = TTF_STYLE_NORMAL;
    }
    else
    {
	if (bold)
	    flag |= TTF_STYLE_BOLD;
	if (italic)
	    flag |= TTF_STYLE_ITALIC;
	if (underline)
	    flag |= TTF_STYLE_UNDERLINE;
    }

    /* Application du style contenu dans le flag à la variable de police */
    TTF_SetFontStyle(font,flag);

    /* Creation de la surface en fonction du type d'écriture et de l'encodage */
    if (type == SOLID)
    {
	switch (encode)
	{
	case LATIN:
	    res = TTF_RenderText_Solid(font, texte, *coul);
	    break;
	case UTF8:
	    res = TTF_RenderUTF8_Solid(font, texte, *coul);
	    break;
	}
    }
    else if (type == SHADED)
    {
	switch (encode)
	{
	case LATIN:
	    res = TTF_RenderText_Shaded(font, texte, *coul, *fond);
	    break;
	case UTF8:
	    res = TTF_RenderUTF8_Shaded(font, texte, *coul, *fond);
	    break;
	}
    }
    else if (type == BLENDED)
    {
	switch (encode)
	{
	case LATIN:
	    res = TTF_RenderText_Blended(font, texte, *coul);
	    break;
	case UTF8:
	    res = TTF_RenderUTF8_Blended(font, texte, *coul);
	    break;
	}
    }

    return res;
}
