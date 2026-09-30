'use client'

import { useId } from 'react'

interface RangeSliderProps {
  label: string
  value: number
  min: number
  max: number
  step?: number
  onChange: (value: number) => void
  /** Formatted readout shown beside the label, e.g. "150,000 cfs" */
  valueText: string
  /** Optional tick labels rendered under the track */
  ticks?: { value: number; label: string }[]
  /** Fill color for the active part of the track */
  accent?: string
}

/** Accessible native range input with the site's track + thumb styling. */
export default function RangeSlider({
  label,
  value,
  min,
  max,
  step = 1,
  onChange,
  valueText,
  ticks,
  accent = '#B87333',
}: RangeSliderProps) {
  const id = useId()
  const pct = ((value - min) / (max - min)) * 100

  return (
    <div>
      <div className="flex items-baseline justify-between gap-4 mb-3">
        <label htmlFor={id} className="text-sm text-titanium">
          {label}
        </label>
        <output htmlFor={id} className="text-sm font-semibold text-white tabular-nums">
          {valueText}
        </output>
      </div>
      <input
        id={id}
        type="range"
        min={min}
        max={max}
        step={step}
        value={value}
        onChange={(e) => onChange(Number(e.target.value))}
        aria-valuetext={valueText}
        className="range-slider w-full"
        style={{
          background: `linear-gradient(to right, ${accent} 0%, ${accent} ${pct}%, rgba(255,255,255,0.08) ${pct}%, rgba(255,255,255,0.08) 100%)`,
        }}
      />
      {ticks && (
        <div className="relative mt-2 h-4 text-[11px] text-muted">
          {ticks.map((t) => {
            const left = ((t.value - min) / (max - min)) * 100
            const align = left < 8 ? 'translate-x-0' : left > 92 ? '-translate-x-full' : '-translate-x-1/2'
            return (
              <span key={t.value} className={`absolute top-0 whitespace-nowrap ${align}`} style={{ left: `${left}%` }}>
                {t.label}
              </span>
            )
          })}
        </div>
      )}
    </div>
  )
}
