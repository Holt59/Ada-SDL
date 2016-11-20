/******************************************/
/*                                        */
/*         PACKAGE GAME - PARTIE C        */
/*                                        */
/*       Gestion du dessin sur surface    */
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

/* Retourne un pixel sous la forme d'un Uint32*/
Uint32 SDL_getpixel(SDL_Surface *surface, int x, int y);

/* Change un pixel de la surface surface par le pixel contenu dans un Uint32*/
void SDL_putpixel(SDL_Surface *surface, int x, int y, Uint32 pixel);

/* Retourne le pixel de la surface "s" se trouvant en (x,y) */
SDL_Color get_pixel(SDL_Surface *s, int x, int y);

/* Change le pixel de la surface "s" se trouvant en (x,y) avec la nouvelle couleur "coul" */
void put_pixel(SDL_Surface *s, int x, int y, SDL_Color *coul);

/* Bloque une surface */
void lock_surface(SDL_Surface *s);

/* Debloque une surface */
void unlock_surface(SDL_Surface *s);


/*********/
/* CORPS */
/*********/

/*    SDL_GETPIXEL     */
/* (Cf. doc de la SDL) */

Uint32 SDL_getpixel(SDL_Surface *surface, int x, int y)
{
    int bpp = surface->format->BytesPerPixel;
    Uint8 *p = (Uint8 *)surface->pixels + y * surface->pitch + x * bpp;

    switch(bpp) {
    case 1:
        return *p;

    case 2:
        return *(Uint16 *)p;

    case 3:
        if(SDL_BYTEORDER == SDL_BIG_ENDIAN)
            return p[0] << 16 | p[1] << 8 | p[2];
        else
            return p[0] | p[1] << 8 | p[2] << 16;

    case 4:
        return *(Uint32 *)p;

    default:
        return 0;
    }
}

/*    SDL_PUTPIXEL     */
/* (Cf. doc de la SDL) */

void SDL_putpixel(SDL_Surface *surface, int x, int y, Uint32 pixel)
{
    int bpp = surface->format->BytesPerPixel;
    Uint8 *p = (Uint8 *)surface->pixels + y * surface->pitch + x * bpp;

    switch(bpp) {
    case 1:
        *p = pixel;
        break;

    case 2:
        *(Uint16 *)p = pixel;
        break;

    case 3:
        if(SDL_BYTEORDER == SDL_BIG_ENDIAN) {
            p[0] = (pixel >> 16) & 0xff;
            p[1] = (pixel >> 8) & 0xff;
            p[2] = pixel & 0xff;
        } else {
            p[0] = pixel & 0xff;
            p[1] = (pixel >> 8) & 0xff;
            p[2] = (pixel >> 16) & 0xff;
        }
        break;

    case 4:
        *(Uint32 *)p = pixel;
        break;
    }
}

/* GET_PIXEL */

SDL_Color get_pixel(SDL_Surface *s, int x, int y)
{
    /* On récupère la couleur sous forme d'un Uint32 */
    Uint32 coul = SDL_getpixel(s,x,y);
    SDL_Color res;
    /* On récupère chaque composante de l'uint32 dans les composantes du résultat */
    SDL_GetRGB(coul,s->format,&(res.r),&(res.g),&(res.b));
    /* On met à 0 la composante inutile du résultat */
    res.unused = 0;
    return res;
}

/* PUT_PIXEL */

void put_pixel(SDL_Surface *s, int x, int y, SDL_Color *coul)
{
    /* On appelle la fonction SDL_putpixel en lui fournissant un Uint32 crée à partir de "coul" */
    SDL_putpixel(s,x,y,SDL_MapRGB(s->format,coul->r,coul->g,coul->b));
}

/* LOCK_SURFACE */

void lock_surface(SDL_Surface *s)
{
    /* Si la surface doit être bloqué, on la bloque */
    if (SDL_MUSTLOCK(s))
	SDL_LockSurface(s);
}

/* UNLOCK_SURFACE */

void unlock_surface(SDL_Surface *s)
{
    if (SDL_MUSTLOCK(s))
	SDL_UnlockSurface(s);
}
