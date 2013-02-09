#!/usr/bin/env python

import errno
import guessit
import itertools
import logging
import os
import re
import shutil
import sys
import time
import UnRAR2
#import pynma

DEFAULT_DIR = '/mnt/torrent'
#TV_DIR = '/data/tv'
TV_DIR = '/glftpd/site/TvShows'
#MOVIE_DIR = '/data/movie'
MOVIE_DIR = '/glftpd/site/Movies'
LOG_DIR = '/home/stefan/logs'
FILE_PATTERNS = ('^(?!.*sample).*\.mkv$',
                 '^((?!\.part(?!0*1\.rar$)\d+\.rar$).)*\.(?:rar)$')

class Torrent:
    def __init__(self, directory, name):
        logging.info('Processing torrent: %s', name)
        self.directory = directory
        self.name = name
        self.episodes = list()
        self.movies   = list()
        self.files    = list()
        for root, dirs, files in os.walk(os.path.join(directory, name)):
            for filename in files:
                for pattern in FILE_PATTERNS:
                    if re.match(pattern, filename.lower()):
                        self.analyse(os.path.join(root, filename))


    def analyse(self, filename):
        if os.path.splitext(filename)[1] == '.rar':
            rar = UnRAR2.RarFile(filename)
            for fn in rar.infoiter():
                self.analyse2(RarFile(os.path.join(os.path.dirname(filename), fn.filename), rar))
        else:
            self.analyse2(File(filename))

    def analyse2(self, file):
        guess = guessit.guess_video_info(unicode(file), info = ['filename'])
        if Episode.isEpisode(guess):
            episode = Episode(guess, file)
            logging.info('Looks like a TV series episode: %s', episode)
            self.episodes.append(episode)
        elif Movie.isMovie(file, guess):
            movie = Movie(guess, file)
            logging.info('Looks like a movie: %s', movie)
            self.movies.append(movie)
        else:
            logging.error('Unhandled file type: %s', file)

    def getEpisodes(self):
        return self.episodes

    def getFiles(self):
        return self.files

    def getMovies(self):
        return self.movies

class File:
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name

    def __str__(self):
        return self.name

    def dirname(self):
        return os.path.basename(os.path.dirname(self.name))

    def lower(self):
        return self.name.lower()

    def transfer(self, directory):
        mkdir_p(directory)
        shutil.copy2(self.name, directory)

class RarFile():
    def __init__(self, filename, rarfile):
        self.filename = filename
        self.rarfile = rarfile

    def __repr__(self):
        return self.filename

    def __str__(self):
        return self.filename

    def dirname(self):
        return os.path.basename(os.path.dirname(self.filename))

    def lower(self):
        return self.filename.lower()

    def transfer(self, directory):
        self.rarfile.extract(self.filename, directory)

class Episode:
    @staticmethod
    def isEpisode(guess):
        return (guess['type'] == 'episode' and 'series' in guess and 'season' in guess and 'episodeNumber' in guess)

    def __init__(self, guess, file):
        self.guess = guess
        self.file = file

    def __repr__(self):
        return '%s[S%02dE%02d]' % (self.getSeries(), self.getSeason(), self.getEpisode())

    def getDestinationDirectory(self):
        series_name = re.sub(" ", ".", self.guess['series'])
        if 'year' in self.guess:
            return os.path.join(TV_DIR, re.sub(" ", ".", '%s.%s/S%02d' % (series_name, self.guess['year'], self.getSeason())))
        else:
            return os.path.join(TV_DIR, re.sub(" ", ".", '%s/S%02d' % (series_name, self.getSeason())))

    def getEpisode(self):
        return self.guess['episodeNumber']

    def getFile(self):
        return self.file

    def getSeason(self):
        return self.guess['season']

    def getSeries(self):
        if 'year' in self.guess:
            return '%s (%d)' % (self.guss['series'], self.guess['year'])
        else:
            return self.guess['series']

    def transfer(self):
        self.file.transfer(self.getDestinationDirectory())

class Movie:
    @staticmethod
    def isMovie(file, guess):
        return (guess['type'] == 'movie' and not 'xxx' in file.lower())

    def __init__(self, guess, file):
        self.guess = guess
        self.file = file

    def __repr__(self):
        if 'year' in self.guess:
            return '%s (%d)' % (self.guess['title'], self.guess['year'])
        else:
            return self.guess['title']

    def getDestinationDirectory(self):
        return os.path.join(MOVIE_DIR, self.file.dirname())

    def getFile(self):
        return self.file

    def transfer(self):
        self.file.transfer(self.getDestinationDirectory())

def mkdir_p(path):
    try:
        os.makedirs(path)
    except OSError as exc: # Python >2.5
        if exc.errno == errno.EEXIST and os.path.isdir(path):
            pass
        else: raise

#def notify(event, description):
#    nma = pynma.PyNMA(NMA_API_KEY)
#    nma.push('Media Organizer', event, description)

def main():
    logging.basicConfig(format='%(asctime)s %(levelname)s:%(message)s',
                        filename=os.path.join(LOG_DIR, 'media.log'),
                        level=logging.INFO)

    if len(sys.argv) == 2:
        name = os.path.basename(sys.argv[1])
        dir = os.path.dirname(sys.argv[1])
        if len(dir) == 0:
            dir = DEFAULT_DIR
    else:
        # Executed by deluge
        name = sys.argv[2]
        dir  = sys.argv[3]

    try:
        torrent = Torrent(dir, name)

        for episode in torrent.getEpisodes():
            episode.transfer()
            logging.info('Transferred %s to %s', episode.getFile(), episode.getDestinationDirectory())

        for movie in torrent.getMovies():
            movie.transfer()
            logging.info('Transferred %s to %s', movie.getFile(), movie.getDestinationDirectory())

    except: # catch *all* exceptions
        logging.error(sys.exc_info())

if __name__ == "__main__":
    main()
