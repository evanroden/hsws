type ClassValue = string | number | boolean | undefined | null | Record<string, boolean | undefined | null> | ClassValue[]

export function cn(...inputs: ClassValue[]): string {
  const classes: string[] = []
  for (const arg of inputs.flat() as ClassValue[]) {
    if (!arg) continue
    if (typeof arg === 'string') {
      classes.push(arg)
    } else if (typeof arg === 'number') {
      classes.push(String(arg))
    } else if (typeof arg === 'object' && !Array.isArray(arg)) {
      for (const [key, value] of Object.entries(arg as Record<string, boolean>)) {
        if (value) classes.push(key)
      }
    }
  }
  return classes.join(' ')
}

export function formatNumber(num: number): string {
  if (num >= 1000000) return `${(num / 1000000).toFixed(1)}M`
  if (num >= 1000) return `${(num / 1000).toFixed(0)}K`
  return num.toString()
}
