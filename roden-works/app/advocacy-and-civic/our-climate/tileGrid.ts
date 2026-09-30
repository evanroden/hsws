/**
 * Square tile grid for the 50 states + DC, following NPR's square tile grid map
 * as published in "Let's tesselate: Hexagons for tile grid maps" (NPR Visuals,
 * 2015-05-11, https://blog.apps.npr.org/2015/05/11/hex-tile-maps — figure
 * "A square tile grid map"). Positions were read tile-by-tile from that figure:
 * 12 columns × 8 rows, zero-based; Alaska and Hawaii sit alone in column 0.
 */
export interface Tile {
  abbr: string
  name: string
  col: number
  row: number
}

export const TILE_COLS = 12
export const TILE_ROWS = 8

export const TILE_GRID: Tile[] = [
  { abbr: 'AK', name: 'Alaska', col: 0, row: 0 },
  { abbr: 'ME', name: 'Maine', col: 11, row: 0 },

  { abbr: 'VT', name: 'Vermont', col: 10, row: 1 },
  { abbr: 'NH', name: 'New Hampshire', col: 11, row: 1 },

  { abbr: 'WA', name: 'Washington', col: 1, row: 2 },
  { abbr: 'ID', name: 'Idaho', col: 2, row: 2 },
  { abbr: 'MT', name: 'Montana', col: 3, row: 2 },
  { abbr: 'ND', name: 'North Dakota', col: 4, row: 2 },
  { abbr: 'MN', name: 'Minnesota', col: 5, row: 2 },
  { abbr: 'IL', name: 'Illinois', col: 6, row: 2 },
  { abbr: 'WI', name: 'Wisconsin', col: 7, row: 2 },
  { abbr: 'MI', name: 'Michigan', col: 8, row: 2 },
  { abbr: 'NY', name: 'New York', col: 9, row: 2 },
  { abbr: 'RI', name: 'Rhode Island', col: 10, row: 2 },
  { abbr: 'MA', name: 'Massachusetts', col: 11, row: 2 },

  { abbr: 'OR', name: 'Oregon', col: 1, row: 3 },
  { abbr: 'NV', name: 'Nevada', col: 2, row: 3 },
  { abbr: 'WY', name: 'Wyoming', col: 3, row: 3 },
  { abbr: 'SD', name: 'South Dakota', col: 4, row: 3 },
  { abbr: 'IA', name: 'Iowa', col: 5, row: 3 },
  { abbr: 'IN', name: 'Indiana', col: 6, row: 3 },
  { abbr: 'OH', name: 'Ohio', col: 7, row: 3 },
  { abbr: 'PA', name: 'Pennsylvania', col: 8, row: 3 },
  { abbr: 'NJ', name: 'New Jersey', col: 9, row: 3 },
  { abbr: 'CT', name: 'Connecticut', col: 10, row: 3 },

  { abbr: 'CA', name: 'California', col: 1, row: 4 },
  { abbr: 'UT', name: 'Utah', col: 2, row: 4 },
  { abbr: 'CO', name: 'Colorado', col: 3, row: 4 },
  { abbr: 'NE', name: 'Nebraska', col: 4, row: 4 },
  { abbr: 'MO', name: 'Missouri', col: 5, row: 4 },
  { abbr: 'KY', name: 'Kentucky', col: 6, row: 4 },
  { abbr: 'WV', name: 'West Virginia', col: 7, row: 4 },
  { abbr: 'VA', name: 'Virginia', col: 8, row: 4 },
  { abbr: 'MD', name: 'Maryland', col: 9, row: 4 },
  { abbr: 'DE', name: 'Delaware', col: 10, row: 4 },

  { abbr: 'AZ', name: 'Arizona', col: 2, row: 5 },
  { abbr: 'NM', name: 'New Mexico', col: 3, row: 5 },
  { abbr: 'KS', name: 'Kansas', col: 4, row: 5 },
  { abbr: 'AR', name: 'Arkansas', col: 5, row: 5 },
  { abbr: 'TN', name: 'Tennessee', col: 6, row: 5 },
  { abbr: 'NC', name: 'North Carolina', col: 7, row: 5 },
  { abbr: 'SC', name: 'South Carolina', col: 8, row: 5 },
  { abbr: 'DC', name: 'District of Columbia', col: 9, row: 5 },

  { abbr: 'OK', name: 'Oklahoma', col: 4, row: 6 },
  { abbr: 'LA', name: 'Louisiana', col: 5, row: 6 },
  { abbr: 'MS', name: 'Mississippi', col: 6, row: 6 },
  { abbr: 'AL', name: 'Alabama', col: 7, row: 6 },
  { abbr: 'GA', name: 'Georgia', col: 8, row: 6 },

  { abbr: 'HI', name: 'Hawaii', col: 0, row: 7 },
  { abbr: 'TX', name: 'Texas', col: 4, row: 7 },
  { abbr: 'FL', name: 'Florida', col: 9, row: 7 },
]
