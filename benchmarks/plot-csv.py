#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import html
import math
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


@dataclass(frozen=True)
class Sample:
    size: int
    loom_ms: float
    c_ms: float


@dataclass(frozen=True)
class Series:
    label: str
    color: str
    samples: list[tuple[float, float]]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Plot Loom/OpenMP timing results from benchmark CSV exports.",
    )
    parser.add_argument(
        "paths",
        nargs="*",
        help="CSV files or directories containing CSV files. Defaults to benchmarks/data/*.csv.",
    )
    parser.add_argument(
        "--output-dir",
        default="benchmarks/data/plots",
        help="Directory for generated SVG plots (default: benchmarks/data/plots).",
    )
    return parser.parse_args()


def collect_csv_paths(paths: list[str]) -> list[Path]:
    if not paths:
        base = Path("benchmarks/data")
        return sorted(base.glob("*.csv"))

    result: list[Path] = []
    for raw in paths:
        path = Path(raw)
        if path.is_dir():
            result.extend(sorted(path.glob("*.csv")))
        else:
            result.append(path)
    return sorted(dict.fromkeys(result))


def read_csv(path: Path) -> tuple[str, list[Sample]]:
    with path.open(newline="") as handle:
        reader = csv.DictReader(handle)
        required = {"benchmark", "size", "loom_avg_ms", "c_avg_ms"}
        if reader.fieldnames is None or not required.issubset(reader.fieldnames):
            missing = ", ".join(sorted(required - set(reader.fieldnames or [])))
            raise ValueError(f"{path}: missing required columns: {missing}")

        benchmark_name = ""
        samples: list[Sample] = []
        for row in reader:
            benchmark = row["benchmark"].strip()
            if not benchmark_name:
                benchmark_name = benchmark
            size = int(row["size"])
            loom_ms = float(row["loom_avg_ms"])
            c_ms = float(row["c_avg_ms"])
            samples.append(Sample(size=size, loom_ms=loom_ms, c_ms=c_ms))

    if not samples:
        raise ValueError(f"{path}: no data rows found")

    samples.sort(key=lambda sample: sample.size)
    return benchmark_name or path.stem, samples


def linear_scale(domain_min: float, domain_max: float, range_min: float, range_max: float):
    if domain_max == domain_min:
        domain_max = domain_min + 1.0

    span = domain_max - domain_min
    range_span = range_max - range_min

    def scale(value: float) -> float:
        return range_min + (value - domain_min) * range_span / span

    return scale


def format_size(value: float) -> str:
    return f"{int(round(value)):,}"


def format_ms(value: float) -> str:
    if abs(value) >= 100:
      return f"{value:,.1f}"
    if abs(value) >= 10:
        return f"{value:,.2f}"
    return f"{value:,.3f}"


def svg_escape(text: str) -> str:
    return html.escape(text, quote=True)


def text(x: float, y: float, content: str, *, size: int = 14, anchor: str = "start", weight: str = "normal") -> str:
    return (
        f'<text x="{x:.2f}" y="{y:.2f}" '
        f'font-size="{size}" font-weight="{weight}" text-anchor="{anchor}" '
        f'font-family="system-ui, -apple-system, Segoe UI, Roboto, sans-serif">'
        f"{svg_escape(content)}</text>"
    )


def polyline(points: Iterable[tuple[float, float]], color: str, width: int = 3) -> str:
    joined = " ".join(f"{x:.2f},{y:.2f}" for x, y in points)
    return f'<polyline fill="none" stroke="{color}" stroke-width="{width}" points="{joined}" />'


def circles(points: Iterable[tuple[float, float]], color: str, radius: int = 4) -> list[str]:
    return [
        f'<circle cx="{x:.2f}" cy="{y:.2f}" r="{radius}" fill="{color}" stroke="white" stroke-width="1.5" />'
        for x, y in points
    ]


def render_plot(csv_path: Path, benchmark: str, samples: list[Sample], output_dir: Path) -> Path:
    width = 1100
    height = 700
    margin_left = 90
    margin_right = 35
    margin_top = 75
    margin_bottom = 95
    plot_width = width - margin_left - margin_right
    plot_height = height - margin_top - margin_bottom

    sizes = [sample.size for sample in samples]
    loom_values = [sample.loom_ms for sample in samples]
    c_values = [sample.c_ms for sample in samples]

    x_min = min(sizes)
    x_max = max(sizes)
    y_min = 0.0
    y_max = max(max(loom_values), max(c_values))
    y_max = max(y_max * 1.12, 1.0)

    x_scale = linear_scale(float(x_min), float(x_max), float(margin_left), float(margin_left + plot_width))
    y_scale = linear_scale(y_min, y_max, float(margin_top + plot_height), float(margin_top))

    loom_points = [(x_scale(sample.size), y_scale(sample.loom_ms)) for sample in samples]
    c_points = [(x_scale(sample.size), y_scale(sample.c_ms)) for sample in samples]

    x_ticks = 5
    y_ticks = 5
    x_tick_values = [x_min + (x_max - x_min) * i / max(x_ticks - 1, 1) for i in range(x_ticks)]
    y_tick_values = [y_min + (y_max - y_min) * i / max(y_ticks - 1, 1) for i in range(y_ticks)]

    parts = [
        '<?xml version="1.0" encoding="UTF-8"?>',
        (
            f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" '
            f'viewBox="0 0 {width} {height}">'
        ),
        '<rect x="0" y="0" width="100%" height="100%" fill="white" />',
        text(margin_left, 34, f"{benchmark} timing", size=24, weight="700"),
        text(margin_left, 58, csv_path.name, size=13, weight="500"),
        '<g stroke="#d8d8d8" stroke-width="1">',
    ]

    for value in x_tick_values:
        x = x_scale(value)
        parts.append(f'<line x1="{x:.2f}" y1="{margin_top:.2f}" x2="{x:.2f}" y2="{margin_top + plot_height:.2f}" />')
    for value in y_tick_values:
        y = y_scale(value)
        parts.append(f'<line x1="{margin_left:.2f}" y1="{y:.2f}" x2="{margin_left + plot_width:.2f}" y2="{y:.2f}" />')
    parts.append("</g>")

    parts.append(f'<rect x="{margin_left:.2f}" y="{margin_top:.2f}" width="{plot_width:.2f}" height="{plot_height:.2f}" fill="none" stroke="#808080" stroke-width="1.2" />')

    parts.append(text(margin_left + plot_width / 2, height - 24, "input size", size=16, anchor="middle", weight="600"))
    parts.append(
        f'<text x="22" y="{margin_top + plot_height / 2:.2f}" transform="rotate(-90 22 {margin_top + plot_height / 2:.2f})" '
        f'font-size="16" font-weight="600" text-anchor="middle" font-family="system-ui, -apple-system, Segoe UI, Roboto, sans-serif">avg ms</text>'
    )

    for value in x_tick_values:
        x = x_scale(value)
        parts.append(f'<line x1="{x:.2f}" y1="{margin_top + plot_height:.2f}" x2="{x:.2f}" y2="{margin_top + plot_height + 6:.2f}" stroke="#666" stroke-width="1.2" />')
        parts.append(text(x, margin_top + plot_height + 24, format_size(value), size=12, anchor="middle"))

    for value in y_tick_values:
        y = y_scale(value)
        parts.append(f'<line x1="{margin_left - 6:.2f}" y1="{y:.2f}" x2="{margin_left:.2f}" y2="{y:.2f}" stroke="#666" stroke-width="1.2" />')
        parts.append(text(margin_left - 12, y + 4, format_ms(value), size=12, anchor="end"))

    parts.extend(
        [
            polyline(loom_points, "#1f77b4"),
            *circles(loom_points, "#1f77b4"),
            polyline(c_points, "#ff7f0e"),
            *circles(c_points, "#ff7f0e"),
        ]
    )

    legend_x = margin_left + plot_width - 210
    legend_y = margin_top + 18
    parts.append(f'<rect x="{legend_x:.2f}" y="{legend_y - 18:.2f}" width="195" height="58" rx="10" fill="white" stroke="#d0d0d0" />')
    parts.append(f'<line x1="{legend_x + 14:.2f}" y1="{legend_y:.2f}" x2="{legend_x + 54:.2f}" y2="{legend_y:.2f}" stroke="#1f77b4" stroke-width="3" />')
    parts.append(f'<circle cx="{legend_x + 34:.2f}" cy="{legend_y:.2f}" r="4" fill="#1f77b4" stroke="white" stroke-width="1.2" />')
    parts.append(text(legend_x + 64, legend_y + 5, "Loom", size=13, weight="600"))
    parts.append(f'<line x1="{legend_x + 14:.2f}" y1="{legend_y + 26:.2f}" x2="{legend_x + 54:.2f}" y2="{legend_y + 26:.2f}" stroke="#ff7f0e" stroke-width="3" />')
    parts.append(f'<circle cx="{legend_x + 34:.2f}" cy="{legend_y + 26:.2f}" r="4" fill="#ff7f0e" stroke="white" stroke-width="1.2" />')
    parts.append(text(legend_x + 64, legend_y + 31, "OpenMP", size=13, weight="600"))

    footer = (
        f"n={len(samples)} points, size range {format_size(x_min)}–{format_size(x_max)}, "
        f"timing range 0–{format_ms(y_max)} ms"
    )
    parts.append(text(margin_left, height - 48, footer, size=12, weight="500"))
    parts.append("</svg>")

    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / f"{csv_path.stem}.svg"
    output_path.write_text("\n".join(parts) + "\n", encoding="utf-8")
    return output_path


def main() -> int:
    args = parse_args()
    csv_paths = collect_csv_paths(args.paths)
    if not csv_paths:
        raise SystemExit("no CSV files found")

    output_dir = Path(args.output_dir)
    outputs = []
    for csv_path in csv_paths:
        benchmark, samples = read_csv(csv_path)
        outputs.append(render_plot(csv_path, benchmark, samples, output_dir))

    for output in outputs:
        print(output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
