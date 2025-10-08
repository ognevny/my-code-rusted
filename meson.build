project(
    'rusted',
    'rust',
    version: '0.0.0',
    default_options: [
        'rust_std=2024'
    ],
)

subproject('meval')
subproject('rayon')
subproject('regex')
subproject('itertools')
subproject('sfml')

lib = library('rusted', 'src' / 'lib.rs', dependencies: [
        'meval-0.2-rs',
        'rayon-1.11-rs',
        'regex-1.11-rs',
        'itertools-0.14-rs',
        'sfml-0.25-rs',
    ]
)
