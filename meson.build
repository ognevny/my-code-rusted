project(
    'rusted',
    'rust',
    version: '0.0.0',
    meson_version: '>= 1.7.0',
    default_options: ['rust_std=2024'],
)

itertools = dependency('itertools-0.14-rs')
meval = dependency('meval-0.2-rs')
rayon = dependency('rayon-1-rs')
regex = dependency('regex-1-rs')
sfml = dependency('sfml-0.25-rs')

lib = library(
    'rusted',
    'src' / 'lib.rs',
    dependencies: [meval, rayon, regex, itertools, sfml],
)
