/**
 * Order-to-cash flow data for the Odoo page.
 *
 * Every hand-off below restates something the page copy or a module
 * description already says: a sales order triggers inventory reservation,
 * which feeds MRP scheduling, which generates purchase orders, which flow into
 * accounting; Accounting invoices on delivery confirmation; customers pay
 * invoices in the portal. Purchase is not one of the featured modules — it only
 * appears as a generic step so the chain reads end to end.
 */

export type ModuleId = 'sales' | 'inventory' | 'mrp' | 'purchase' | 'accounting' | 'portal'
export type EdgeId = 'reserve' | 'manufacture' | 'purchase' | 'bill' | 'invoice' | 'payment'

export interface ErpModule {
  id: ModuleId
  label: string
  /** Short role shown on the tile */
  role: string
  /** Generic step: shown for context, not one of Evan's featured modules */
  generic?: boolean
  description: string
}

/** In flow order — this is also the keyboard (Tab / arrow) order. */
export const MODULES: ErpModule[] = [
  {
    id: 'sales',
    label: 'Sales',
    role: 'CRM & quotations',
    description:
      'Full CRM and pipeline management, configurable quotation templates, subscription management, and e-commerce integration. The Sales module drives upstream demand signals to Inventory and MRP while Accounting auto-generates invoices on delivery confirmation.',
  },
  {
    id: 'inventory',
    label: 'Inventory',
    role: 'Stock & deliveries',
    description:
      'Real-time warehouse management with barcode scanning, automated replenishment rules, multi-location tracking, lot/serial traceability, and putaway strategies. Inventory connects directly to MRP for demand-driven procurement and to Sales for accurate delivery promises.',
  },
  {
    id: 'mrp',
    label: 'MRP',
    role: 'Manufacturing',
    description:
      'Manufacturing Resource Planning — multi-level bills of materials, work center routing, Master Production Schedule, finite capacity planning, and OEE analysis. Evan implemented MRP for discrete manufacturers transitioning from spreadsheet-based production tracking to integrated workflows with real-time shop floor visibility.',
  },
  {
    id: 'purchase',
    label: 'Purchase',
    role: 'Generic step',
    generic: true,
    description:
      'Shown only to complete the chain: MRP generates purchase orders for the components it is missing, and those purchase orders flow into Accounting. Purchasing is not one of the modules featured on this page.',
  },
  {
    id: 'accounting',
    label: 'Accounting',
    role: 'Invoicing & ledgers',
    description:
      'Analytic accounting with parallel ledger for internal cost tracking, percentage-based distribution across departments, automated bank reconciliation, and multi-currency support. Evan specialized in analytic accounting implementations that gave CFOs visibility into profitability by product line, project, or department.',
  },
  {
    id: 'portal',
    label: 'Customer Portal',
    role: 'Self-service',
    description:
      'Self-service portal for order tracking, invoice payment, support ticket submission, and document sharing. Reduces operational overhead by empowering customers to manage their own accounts — a key expansion revenue driver in food & beverage and retail implementations.',
  },
]

export interface FlowEdge {
  id: EdgeId
  from: ModuleId
  to: ModuleId
  /** What arrives from the source… */
  trigger: string
  /** …and what it becomes downstream */
  action: string
  /** Step caption used by "Trace an order" */
  caption: string
}

/** In trace order. */
export const EDGES: FlowEdge[] = [
  {
    id: 'reserve',
    from: 'sales',
    to: 'inventory',
    trigger: 'Order confirmed',
    action: 'reserve stock',
    caption: 'A confirmed quotation becomes a sales order, and Inventory reserves stock for its delivery.',
  },
  {
    id: 'manufacture',
    from: 'inventory',
    to: 'mrp',
    trigger: 'Shortage',
    action: 'manufacturing order',
    caption: 'Stock can’t cover the whole order, so the shortage becomes a manufacturing order in MRP.',
  },
  {
    id: 'purchase',
    from: 'mrp',
    to: 'purchase',
    trigger: 'Components',
    action: 'purchase order',
    caption: 'MRP generates purchase orders for the components its bill of materials still needs.',
  },
  {
    id: 'bill',
    from: 'purchase',
    to: 'accounting',
    trigger: 'Purchase order',
    action: 'vendor bill',
    caption: 'Those purchase orders flow into Accounting as vendor bills.',
  },
  {
    id: 'invoice',
    from: 'inventory',
    to: 'accounting',
    trigger: 'Delivery',
    action: 'invoice',
    caption: 'Once the order is built and shipped, confirming the delivery generates the customer invoice.',
  },
  {
    id: 'payment',
    from: 'accounting',
    to: 'portal',
    trigger: 'Invoice',
    action: 'portal payment',
    caption: 'The customer tracks the order and pays the invoice through the portal.',
  },
]

export const moduleById = Object.fromEntries(MODULES.map((m) => [m.id, m])) as Record<ModuleId, ErpModule>

export const handoffLabel = (e: FlowEdge) => `${e.trigger} → ${e.action}`
