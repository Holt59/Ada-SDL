/******************************************/
/*                                        */
/*         PACKAGE GAME - PARTIE C        */
/*                                        */
/*   Gestion du son (musique et "chunk")  */
/*                                        */
/* Créateur : CAPELLE Mikaël              */
/* Adresse  : capelle.mikael@gmail.com    */
/*                                        */
/* Dernière Modification : 06 / 06 / 2011 */
/*                                        */
/******************************************/


#include <SDL.h>
#include <SDL_mixer.h>


/*****************/
/* SPECIFICATION */
/*****************/

/***********/
/* MUSIQUE */
/***********/

/* Une seule musique peut être active à la fois */

/* Charge une musique */
Mix_Music* load_music (char *S);

/* Ferme une musique */
void close_music (Mix_Music *M);

/* Lance une musique L fois (L=0 => boucle) */
void play_music (Mix_Music *M, int L);

/* Modifie le volume des musiques */
void set_music_volume(int v);

/* Met en pause la musique */
void pause_music(void);

/* Relance la musique mise en pause */
void resume_music(void);

/* Redémarre la musique */
void restart_music(void);

/* Arrête la musique */
void stop_music(void);

/* Vérifie si la musique est en pause (1 sinon 0) */
int music_paused(void);

/* Vérifie si la musique est en train de jouer */
int music_playing(void);

/***********************/
/* CHUNK (SONS COURTS) */
/***********************/

/* Plusieurs chunk peuvent être joué en même temps sur différents canaux */

/* Charge un chunk */
Mix_Chunk *load_chunk (char *S);

/* Ferme un chunk */
void close_chunk(Mix_Chunk *C);

/* Alloue nb canaux (retourne le nombre de channel alloués) */
int alloc_chan(int nb);

/* Lance un chunk sur le chan nb pendant time fois */
int play_chan(int chan, Mix_Chunk *C, int nb, int time);

/* Met en pause le chan numéro C */
void pause_chan(int c);

/* Relance le chan numéro c */
void resume_chan(int c);

/* Arrête le chan numéro c */
void stop_chan(int c);

/* Regarde si le chan numéro c est en pause */
int chan_paused(int c);

/* Regarde si le chan numéro c est en train de jouer */
int chan_playing(int c);

/* Met à v le volume du chan c */
void set_chan_volume(int c, int v);


/*****************************/
/* CORPS (cf. doc SDL_Mixer) */
/*****************************/

/* Tous les corps sont de simple appel à des fonctions pré-existantes de la librairie SDL_Mixer */

Mix_Music* load_music (char *S)
{
    return Mix_LoadMUS(S);
}

void close_music (Mix_Music *M)
{
    Mix_FreeMusic(M);
}

void play_music (Mix_Music *M, int L)
{
    Mix_PlayMusic(M,L);
}

void set_music_volume(int v)
{
    Mix_VolumeMusic(v);
}

void pause_music(void)
{
    Mix_PauseMusic();
}

void resume_music(void)
{
    Mix_ResumeMusic();
}

void restart_music(void)
{
    Mix_RewindMusic();
}

void stop_music(void)
{
    Mix_HaltMusic();
}

int music_paused(void)
{
    return Mix_PausedMusic();
}

int music_playing(void)
{
    return Mix_PlayingMusic();
}

Mix_Chunk *load_chunk (char *S)
{
    return Mix_LoadWAV(S);
}

void close_chunk(Mix_Chunk *C)
{
    Mix_FreeChunk(C);
}

int alloc_chan(int nb)
{
    return Mix_AllocateChannels(nb);
}

int play_chan(int chan, Mix_Chunk *C, int nb, int time)
{
    Mix_PlayChannelTimed(chan,C,nb,time);
}

void pause_chan(int c)
{
    Mix_Pause(c);
}

void resume_chan(int c)
{
    Mix_Resume(c);
}

void stop_chan(int c)
{
    Mix_HaltChannel(c);
}

int chan_paused(int c)
{
    return Mix_Paused(c);
}

int chan_playing(int c)
{
    return Mix_Playing(c);
}

void set_chan_volume(int c, int v)
{
    Mix_Volume(c,v);
}
