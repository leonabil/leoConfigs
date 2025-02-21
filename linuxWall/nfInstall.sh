#!/bin/bash
declare -a fonts=(
    0xProto
    3270
    Agave
    AnonymousPro
    Arimo
    AurulentSansMono
    BigBlueTerminal
    BitstreamVeraSansMono
    IBMPlexMono
    CascadiaCode
    CascadiaMono
    CodeNewRoman
    ComicShannsMono
    CommitMono
    Cousine
    D2Coding
    DaddyTimeMono
    DepartureMOno
    DejaVuSansMono
    DroidSansMono
    EnvyCodeR
    FantasqueSansMono
    FiraCode
    FiraMono
    GeistMono    
    Go-Mono
    Gohu
    Hack
    Hasklig
    HeavyData
    Hermit
    iA-Writer
    Inconsolata
    InconsolataGo
    InconsolataLGC
    IntelOneMono
    Iosevka
    IosevkaTerm
    IosevkaTermSlab
    JetBrainsMono
    Lekton
    LiberationMono
    Lilex
    MartianMono
    Meslo
    Noto
    Monaspace
    Monofur
    Monoid
    Mononoki
    MPlus
    Noto
    OpenDiyslexic
    Overpass
    ProFont
    ProggyClean
    Recursive
    RobotoMono
    ShareTechMono
    SourceCodePro
    SpaceMono
    NerdFontsSymbolsOnly
    Terminus
    Tinos
    Ubuntu
    UbuntuMono
    UbuntuSans
    VictorMono
    ZedMOno
)
version='3.3.0'
fonts_dir="${HOME}/.local/share/fonts"
if [[ ! -d "$fonts_dir" ]]; then
    mkdir -p "$fonts_dir"
fi
for font in "${fonts[@]}"; do
    zip_file="${font}.zip"
    # download_url="https://github.com/ryanoasis/nerd-fonts/releases/download/v${version}/${zip_file}"
    echo "Installing $zip_file"
    #wget "$download_url"
    unzip "$zip_file" -d "$fonts_dir"
    #rm "$zip_file"
done


find "$fonts_dir" -name '*Windows Compatible*' -delete
fc-cache -fv

