#!/usr/bin/env python

from erlport import Port, Protocol, String
import guessit

class GuessProtocol(Protocol):

	def handle_guess(self, filename):
		print filename
		return guessit.guess_video_info(String(filename), info = ['filename'])

if __name__ == "__main__":
    proto = GuessProtocol()
    proto.run(Port(use_stdio=True))
