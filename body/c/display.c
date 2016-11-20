/******************************************/
/*                                        */
/*         PACKAGE GAME - PARTIE C        */
/*                                        */
/*    Gestion de l'affichage principal    */
/*                                        */
/* Créateur : CAPELLE Mikaël              */
/* Adresse  : capelle.mikael@gmail.com    */
/*                                        */
/* Dernière Modification : xx / xx / 2011 */
/*                                        */
/******************************************/


#include <SDL.h>
#include <SDL_image.h>


/*****************/
/* SPECIFICATION */
/*****************/

/* Libère la mémoire alloué par la Surface s */
void free_surface(SDL_Surface *s);

/* Lance le mode vidéo (une fenêtre de largeur w et de hauteur h) */
SDL_Surface* set_video(int w, int h, int p, int hw, int dbl_buff, int full_screen, int resizable, int no_frame );

/* Récupère la surface écran */
SDL_Surface* get_screen(void) ;

/* Remplie la partie délimité par Re de la surface s avec la couleur (r,g,b) */
int fill(SDL_Surface *s, SDL_Rect *Re, int r, int g, int b, int full);

/* Met à jour l'écran */
void flip(void);

/* Crée un rectangle */
SDL_Surface *create_rectangle(int w, int h, int p, int hw);

/* Converti une surface */
SDL_Surface *convert_surface (SDL_Surface *src, int hw);

/* Charge une image dans une surface */
SDL_Surface *load_image(const char *file_path);

/* Sauve une image au format bmp */
void save_image(SDL_Surface *S, const char *img_name);

/* Colle l'image src sur la surface dst à la position pos, part correspond à la partie de l'image à coller */
int blit_image(SDL_Surface *dst, SDL_Surface *src, SDL_Rect *pos, SDL_Rect *part, int no_pos, int full_image);

/* Place une couleur de transparence */
void set_color_key(SDL_Surface *S, int flag, int r, int g, int b);

/* Rend visible (ou non) le curseur */
void show_cursor(int active);

/* Convertie une surface pour qu'elle est le même format que la surface écran */
SDL_Surface *display_format(SDL_Surface *S, int alpha);


/*********/
/* CORPS */
/*********/

void free_surface(SDL_Surface *s)
{
    SDL_FreeSurface(s);
}

SDL_Surface* set_video(int w, int h, int p, int hw, int dbl_buff, int full_screen, int resizable, int no_frame )
{
    Uint32 flag = 0;

    if (hw)
	flag |= SDL_HWSURFACE;
    else
	flag |= SDL_SWSURFACE;

    if (dbl_buff)
	flag |= SDL_DOUBLEBUF;
    if (full_screen)
	flag |= SDL_FULLSCREEN;
    if (resizable)
	flag |= SDL_RESIZABLE;
    if (no_frame)
	flag |= SDL_NOFRAME;

    return SDL_SetVideoMode(w,h,p,flag);
}

SDL_Surface* get_screen(void)
{
    return SDL_GetVideoSurface();
}

int fill(SDL_Surface *s, SDL_Rect *Re, int r, int g, int b, int full)
{
    SDL_Rect tmp = {Re->x, Re->y, Re->w, Re->h};
    if (!full)
	return SDL_FillRect(s, &tmp, SDL_MapRGB(s->format,r,g,b));
    else
	return SDL_FillRect(s,NULL,SDL_MapRGB(s->format,r,g,b));
}

void flip(void)
{
    SDL_Flip(SDL_GetVideoSurface());
}

SDL_Surface *create_rectangle(int w, int h, int p, int hw)
{
    SDL_Surface *tmp = NULL;
    Uint32 flag = 0;

    if (hw)
	flag = SDL_HWSURFACE;
    else
	flag = SDL_SWSURFACE;

    if ((tmp = SDL_CreateRGBSurface(flag,w,h,p,0,0,0,0)) == NULL)
	return NULL;

    SDL_FillRect(tmp,NULL,SDL_MapRGB(tmp->format,0,0,0));

    return tmp;
}

SDL_Surface *convert_surface (SDL_Surface *src, int hw)
{
    Uint32 flag = 0;

    if (hw)
	flag = SDL_HWSURFACE;
    else
	flag = SDL_SWSURFACE;

    return SDL_ConvertSurface (src,src->format,flag);
}

SDL_Surface *load_image(const char *file_path)
{
    return IMG_Load(file_path);
}

void save_image(SDL_Surface *S, const char *img_name)
{
    SDL_SaveBMP(S,img_name);
}

int blit_image(SDL_Surface *dst, SDL_Surface *src, SDL_Rect *pos, SDL_Rect *part, int no_pos, int full_image)
{
    SDL_Rect tmp1 = {pos->x, pos->y, pos->w, pos->h};
    SDL_Rect tmp2 = {part->x, part->y, part->w, part->h};
    SDL_Rect *t1 = NULL, *t2 = NULL;

    if (!full_image)
	t2 = &tmp2;
    if (!no_pos)
	t1 = &tmp1;
    
    return SDL_BlitSurface(src,t2,dst,t1) == 0;

}

void set_color_key(SDL_Surface *S, int flag, int r, int g, int b)
{
    if (flag == 1)
	flag = SDL_SRCCOLORKEY;
    else
	flag = 0;

    SDL_SetColorKey(S,flag,SDL_MapRGB(S->format,r,g,b));
}

void show_cursor(int active)
{
    if (active)
	SDL_ShowCursor(SDL_ENABLE);
    else
	SDL_ShowCursor(SDL_DISABLE);
}

SDL_Surface *display_format(SDL_Surface *S, int alpha)
{
    if (S != NULL)
    {
	if (alpha)
	    return SDL_DisplayFormatAlpha(S);
	else
	    return SDL_DisplayFormat(S);
    }
    else
	return NULL;
}
