'use client'

interface SegmentedControlProps<T extends string> {
  options: { value: T; label: string }[]
  value: T
  onChange: (value: T) => void
  label: string
  size?: 'sm' | 'md'
}

/** Single-choice toggle used for chart views and diagram modes across the site. */
export default function SegmentedControl<T extends string>({
  options,
  value,
  onChange,
  label,
  size = 'sm',
}: SegmentedControlProps<T>) {
  return (
    <div
      role="group"
      aria-label={label}
      className="inline-flex rounded-lg border border-white/[0.08] bg-white/[0.03] p-0.5"
    >
      {options.map((opt) => {
        const active = opt.value === value
        return (
          <button
            key={opt.value}
            type="button"
            aria-pressed={active}
            onClick={() => onChange(opt.value)}
            className={`rounded-md font-medium transition-colors duration-200 ${
              size === 'sm' ? 'px-3 py-1.5 text-xs' : 'px-4 py-2 text-sm'
            } ${active ? 'bg-white/[0.1] text-white shadow-sm' : 'text-muted hover:text-white'}`}
          >
            {opt.label}
          </button>
        )
      })}
    </div>
  )
}
