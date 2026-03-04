'use client'

import { useCountUp } from '@/lib/hooks'

interface StatCounterProps {
  value: number
  prefix?: string
  suffix?: string
  label: string
  duration?: number
}

export default function StatCounter({
  value,
  prefix = '',
  suffix = '',
  label,
  duration = 2000,
}: StatCounterProps) {
  const { count, ref } = useCountUp(value, duration)

  const displayValue = Number.isInteger(value)
    ? Math.round(count)
    : count.toFixed(1)

  // Build the full accessible value string (e.g. "$143.8M")
  const fullValue = `${prefix}${Number.isInteger(value) ? value : value.toFixed(1)}${suffix}`

  return (
    <div className="text-center px-6 py-4" role="group" aria-label={label}>
      <span
        ref={ref}
        className="block font-serif text-3xl md:text-4xl text-white"
        aria-label={`${fullValue} ${label}`}
        aria-live="polite"
      >
        {prefix}
        {displayValue}
        {suffix}
      </span>
      <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase" aria-hidden="true">
        {label}
      </span>
    </div>
  )
}
