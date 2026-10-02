'use client'

import { useCountUp } from '@/lib/hooks'

interface StatCounterProps {
  value: number
  prefix?: string
  suffix?: string
  label: string
  duration?: number
  /** Footnote marker rendered after the value, e.g. <Cite sources={S} id="x" /> */
  cite?: React.ReactNode
}

export default function StatCounter({
  value,
  prefix = '',
  suffix = '',
  label,
  duration = 2000,
  cite,
}: StatCounterProps) {
  const { count, ref } = useCountUp(value, duration)

  const displayValue = Number.isInteger(value)
    ? Math.round(count).toLocaleString('en-US')
    : count.toFixed(1)

  // Build the full accessible value string (e.g. "$143.8M")
  const fullValue = `${prefix}${Number.isInteger(value) ? value.toLocaleString('en-US') : value.toFixed(1)}${suffix}`

  return (
    <div className="text-center px-6 py-4" role="group" aria-label={label}>
      <span
        ref={ref}
        className="block font-sans font-semibold tracking-tight text-3xl md:text-[2.5rem] leading-none text-white"
        aria-label={`${fullValue} ${label}`}
        aria-live="polite"
      >
        {prefix}
        {displayValue}
        {suffix}
        {cite && <span className="text-[1.1rem] align-top">{cite}</span>}
      </span>
      <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase" aria-hidden="true">
        {label}
      </span>
    </div>
  )
}
