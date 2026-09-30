import type { ReactElement } from 'react'
import type { ModuleId } from './erpFlow'

/** One consistent line-icon set for the ERP modules: 24px grid, 1.6px stroke, round joins. */
export default function ModuleIcon({ id, className = 'w-5 h-5' }: { id: ModuleId; className?: string }) {
  return (
    <svg
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth={1.6}
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
      className={className}
    >
      {ICONS[id]}
    </svg>
  )
}

const ICONS: Record<ModuleId, ReactElement> = {
  // Price tag
  sales: (
    <>
      <path d="M3.5 11.9V4.9c0-.77.63-1.4 1.4-1.4h7l8.6 8.6-8.4 8.4-8.6-8.6Z" />
      <circle cx="8.2" cy="8.2" r="1.4" />
    </>
  ),
  // Package
  inventory: (
    <>
      <path d="M12 3.2 19.8 7.5v9L12 20.8l-7.8-4.3v-9L12 3.2Z" />
      <path d="M4.5 7.7 12 11.9l7.5-4.2M12 11.9v8.6" />
    </>
  ),
  // Factory with chimney
  mrp: (
    <>
      <path d="M3.5 20.5v-9.6l4.6 2.9v-2.9l4.6 2.9v-2.9l4 2.5V3.8h3.8v16.7H3.5Z" />
      <path d="M7 17.2h1.6M11.6 17.2h1.6" />
    </>
  ),
  // Cart
  purchase: (
    <>
      <path d="M3 4.2h2.1l2.2 10a1.2 1.2 0 0 0 1.17.94h8.05a1.2 1.2 0 0 0 1.16-.9l1.6-6.34H6.05" />
      <circle cx="9.4" cy="19" r="1.35" />
      <circle cx="16.4" cy="19" r="1.35" />
    </>
  ),
  // Calculator
  accounting: (
    <>
      <rect x="5" y="3" width="14" height="18" rx="2.2" />
      <rect x="8" y="6" width="8" height="3.6" rx="0.7" />
      {[8.6, 12, 15.4].flatMap((cx) =>
        [13.3, 16.8].map((cy) => <circle key={`${cx}-${cy}`} cx={cx} cy={cy} r="0.95" fill="currentColor" stroke="none" />)
      )}
    </>
  ),
  // Browser window with a customer
  portal: (
    <>
      <rect x="3" y="4" width="18" height="16" rx="2.2" />
      <path d="M3 8.4h18" />
      <circle cx="12" cy="12.4" r="1.9" />
      <path d="M8.5 17.6a3.6 3.6 0 0 1 7 0" />
    </>
  ),
}
