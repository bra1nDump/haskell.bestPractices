FROM phadej/ghc:8.0.2-ubuntu
MAINTAINER Kirill Dubovitskiy <brainddump@ucla.edu>

RUN apt-get update
RUN apt-get install -y sudo

RUN  useradd box -m && echo "box:box" | chpasswd && adduser box sudo
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

USER box
WORKDIR /home/box
RUN chown box:sudo .

RUN sudo apt-get install -y emacs
RUN git clone https://github.com/syl20bnr/spacemacs .emacs.d

# COPY .spacemacs .spacemacs

VOLUME ["/home/box"]

CMD ["/bin/bash"]

